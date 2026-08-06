#import "result.typ": *
#import "spec.typ": *
#import "std.typ" as std

/// Validates function call arguments against an argument spec.
///
/// This lower-level variant accepts the recursive validator to use.
/// -> RESULT(arguments)
#let validate-args-aux(
  /// Validator to use recursively.
  /// -> function
  validate,
  /// Argument spec.
  /// -> args-spec
  args-spec,
  /// Arguments to validate.
  /// -> arguments
  ..args,
) = {
  let normalized-args-spec = result-unwrap(args-spec-parse(args-spec))
  args-spec-elim(
    null: {
      if args.pos().len() != 0 or args.named().len() != 0 {
        err(
          "expected no arguments",
          trace: (__tag__: "trace/args-null", extra-args: args),
        )
      } else {
        ok(())
      }
    },
    args: (..args-spec) => {
      if args.pos().len() != args-spec.pos().len() {
        return err(
          "expected "
            + str(args-spec.pos().len())
            + " positional argument(s), got "
            + str(args.pos().len()),
          trace: (
            __tag__: "trace/args-arity",
            args-spec: normalized-args-spec,
            value: args,
          ),
        )
      }
      let pos = ()
      for (index, (pos-spec, pos-value)) in args-spec
        .pos()
        .zip(args.pos())
        .enumerate() {
        let result = validate(pos-spec, pos-value)
        if result-is-err(result) {
          return result-trace(
            cont => (
              __tag__: "trace/args-pos-arg",
              args-spec: normalized-args-spec,
              index: index,
              value: pos-value,
              cont: cont,
            ),
            result,
          )
        }
        pos.push(result.value)
      }
      let named-args = args.named()
      let named-spec-value = (:)
      let missing-named-args = (:)
      for (arg-name, arg-spec) in args-spec.named().pairs() {
        if named-args.keys().contains(arg-name) {
          let arg-value = named-args.remove(arg-name)
          named-spec-value.insert(arg-name, (arg-spec, arg-value))
        } else {
          missing-named-args.insert(arg-name, arg-spec)
        }
      }
      if named-args.len() > 0 {
        let plural = if named-args.len() > 1 { "s" } else { "" }
        err(
          "unexpected argument" + plural + ": " + repr(named-args.keys()),
          trace: (
            __tag__: "trace/args-extra-named",
            args-spec: normalized-args-spec,
            extra-args: named-args,
          ),
        )
      } else if missing-named-args.len() > 0 {
        let plural = if missing-named-args.len() > 1 { "s" } else { "" }
        err(
          "missing value"
            + plural
            + "for argument"
            + plural
            + ": "
            + repr(missing-named-args.keys()),
          trace: (
            __tag__: "trace/args-missing-named",
            args-spec: normalized-args-spec,
            missing-args: missing-named-args.keys(),
          ),
        )
      } else {
        let named = (:)
        for (arg-name, (arg-spec, arg-value)) in named-spec-value.pairs() {
          let result = validate(arg-spec, arg-value)
          if result-is-err(result) {
            return result-trace(
              cont => (
                __tag__: "trace/args-named-arg",
                args-spec: normalized-args-spec,
                name: arg-name,
                value: arg-value,
                cont: cont,
              ),
              result,
            )
          }
          named.insert(arg-name, result.value)
        }
        ok(arguments(..pos, ..named))
      }
    },
  )(normalized-args-spec)
}

/// Validates constructor arguments against a constructor spec.
///
/// This lower-level variant accepts the recursive validator to use.
/// -> RESULT(dictionary)
#let validate-constr-aux(
  /// Validator to use recursively.
  /// -> function
  validate,
  /// Constructor spec.
  /// -> constr-spec
  constr-spec,
  /// Constructor arguments to validate.
  /// -> arguments
  ..args,
) = {
  let normalized-constr-spec = result-unwrap(constr-spec-parse(constr-spec))
  constr-spec-elim(
    null: {
      let named = args.named()
      if args.pos().len() != 0 or named.len() != 0 {
        err(
          "expected no arguments",
          trace: (
            __tag__: "trace/constr-null",
            constr-spec: normalized-constr-spec,
            value: args,
          ),
        )
      } else {
        ok((:))
      }
    },
    fields: fields-spec => {
      let (pos, named) = (args.pos(), args.named())
      let fields = (:)
      for (field-name, field-spec) in fields-spec.pairs() {
        let arg = if named.keys().contains(field-name) {
          named.remove(field-name)
        } else if pos.len() == 0 {
          return err(
            "not enough arguments",
            trace: (
              __tag__: "trace/constr-missing-arg",
              constr-spec: normalized-constr-spec,
              constr-arg: field-name,
            ),
          )
        } else {
          let (arg, ..new-pos) = pos
          pos = new-pos
          arg
        }
        let result = validate(field-spec, arg)
        if result-is-err(result) {
          return result-trace(
            cont => (
              __tag__: "trace/constr-field",
              constr-spec: normalized-constr-spec,
              constr-arg: field-name,
              value: arg,
              cont: cont,
            ),
            result,
          )
        } else {
          fields.insert(field-name, result.value)
        }
      }
      if pos.len() > 0 or named.len() > 0 {
        let extra-args = arguments(..pos, ..named)
        err(
          "unrecognized arguments: " + repr(extra-args),
          trace: (
            __tag__: "trace/constr-extra-args",
            constr-spec: normalized-constr-spec,
            extra-args: extra-args,
          ),
        )
      } else {
        ok(fields)
      }
    },
  )(normalized-constr-spec)
}

/// Validates a value against a spec.
///
/// Returns `ok(validated-value)` when the value matches the spec, or
/// `err(message)` otherwise.
///
/// Implemented as an iterative worklist machine (a defunctionalized version
/// of the natural recursive validator): pending checks live on an explicit
/// task stack and finished values on a result stack, so validation depth is
/// bounded by heap data instead of the Typst call stack. Deeply nested
/// values — including long recursive chains — validate without hitting the
/// maximum function call depth.
/// -> RESULT(any)
#let validate(
  /// Spec to validate against.
  /// -> spec
  spec,
  /// Value to validate.
  /// -> any
  value,
) = {
  // Attaches `node` as the outermost frame of an error's trace.
  let trace-wrap(node, error) = (
    __tag__: "result/err",
    msg: error.msg,
    trace: (..node, cont: error.trace),
  )
  // Wraps an error with this level's `trace/val` frame, then with the
  // inherited on-error frames of the task that spawned it (innermost first).
  let fail-val(error, spec, value, wraps) = {
    let error = trace-wrap(
      (__tag__: "trace/val", spec: spec, value: value),
      error,
    )
    for node in wraps {
      error = trace-wrap(node, error)
    }
    error
  }

  // Task kinds:
  // - "val": validate `value` against `spec`; `wrap` holds trace frames to
  //   attach if this subtree fails (innermost first).
  // - "build-constr" / "build-array" / "build-dict": assemble the results of
  //   a composite's children back into a value once they are all validated.
  // - "union": choice point; on failure of the active member, roll back and
  //   try the next one.
  // - "fail-missing": a constructor field had no matching argument; fails at
  //   its position so that earlier field errors keep priority.
  let tasks = ((k: "val", spec: spec, value: value, wrap: ()),)
  let results = ()
  let error = none

  while tasks.len() > 0 {
    let top = tasks.pop()

    if error != none {
      // Unwinding: pop pending work, attaching each enclosing level's trace
      // frames, until a union choice point absorbs the error or the task
      // stack runs out.
      if top.k == "build-constr" {
        if top.kind == "enum" {
          error = trace-wrap((__tag__: "trace/constr", name: top.cname), error)
        }
        error = fail-val(error, top.spec, top.value, top.wrap)
      } else if top.k == "build-array" or top.k == "build-dict" {
        error = fail-val(error, top.spec, top.value, top.wrap)
      } else if top.k == "union" {
        let first-error = top.first-error
        if first-error == none {
          first-error = trace-wrap(
            (
              __tag__: "trace/union",
              spec: top.elems.at(top.idx),
              value: top.value,
            ),
            error,
          )
        }
        if top.idx + 1 < top.elems.len() {
          // Try the next union member from a clean slate.
          results = results.slice(0, top.rlen)
          tasks.push((..top, idx: top.idx + 1, first-error: first-error))
          tasks.push((
            k: "val",
            spec: top.elems.at(top.idx + 1),
            value: top.value,
            wrap: (),
          ))
          error = none
        } else {
          error = fail-val(first-error, top.spec, top.value, top.wrap)
        }
      }
      // Plain pending tasks ("val", "fail-missing") belong to subtrees that
      // no longer need checking; they are discarded silently.
      continue
    }

    if top.k == "val" {
      let spec = top.spec
      if std.type(spec) == std.type {
        spec = (__tag__: "spec/builtin", name: str(spec), value: spec)
      }
      if std.type(spec) != std.dictionary {
        panic("ill-formed spec", spec)
      }
      if (
        not spec.keys().contains("__tag__")
          or not spec.__tag__.starts-with("spec/")
      ) {
        panic("dictionary specs must contain a `spec/` tag")
      }
      let value = top.value
      let tag = spec.__tag__
      if tag == "spec/builtin" {
        assert(std.type(spec.value) == std.type)
        if std.type(value) == spec.value {
          results.push(value)
        } else {
          error = fail-val(
            err(
              "expected a value of type `"
                + str(spec.value)
                + "`, got `"
                + str(std.type(value))
                + "`",
            ),
            spec,
            value,
            top.wrap,
          )
        }
      } else if tag == "spec/any" {
        results.push(value)
      } else if tag == "spec/function" {
        if std.type(value) == std.function {
          results.push(value)
        } else {
          error = fail-val(
            err("expected function, got `" + str(std.type(value)) + "`"),
            spec,
            value,
            top.wrap,
          )
        }
      } else if tag == "spec/array" {
        if std.type(value) != std.array {
          error = fail-val(
            err("expected array, got `" + str(std.type(value)) + "`"),
            spec,
            value,
            top.wrap,
          )
        } else {
          tasks.push((
            k: "build-array",
            count: value.len(),
            spec: spec,
            value: value,
            wrap: top.wrap,
          ))
          for i in range(value.len()).rev() {
            tasks.push((
              k: "val",
              spec: spec.inner,
              value: value.at(i),
              wrap: ((
                __tag__: "trace/array-val",
                index: i,
                value: value.at(i),
              ),),
            ))
          }
        }
      } else if tag == "spec/dictionary" {
        if std.type(value) != std.dictionary {
          error = fail-val(
            err("expected dictionary, got `" + str(std.type(value)) + "`"),
            spec,
            value,
            top.wrap,
          )
        } else {
          tasks.push((
            k: "build-dict",
            keys: value.keys(),
            spec: spec,
            value: value,
            wrap: top.wrap,
          ))
          for (key, item) in value.pairs().rev() {
            tasks.push((
              k: "val",
              spec: spec.value,
              value: item,
              wrap: ((
                __tag__: "trace/dictionary-val",
                key: key,
                value: item,
              ),),
            ))
          }
        }
      } else if tag == "spec/union" {
        if spec.elems.len() == 0 {
          error = fail-val(
            err("empty type has no values"),
            spec,
            value,
            top.wrap,
          )
        } else {
          tasks.push((
            k: "union",
            elems: spec.elems,
            idx: 0,
            value: value,
            spec: spec,
            first-error: none,
            rlen: results.len(),
            wrap: top.wrap,
          ))
          tasks.push((
            k: "val",
            spec: spec.elems.first(),
            value: value,
            wrap: (),
          ))
        }
      } else if tag == "spec/enum" {
        if (
          std.type(value) != std.dictionary
            or not value.keys().contains("__tag__")
        ) {
          error = fail-val(
            err("not an enum value: " + repr(value)),
            spec,
            value,
            top.wrap,
          )
        } else {
          let fields-value = value
          let value-tag = fields-value.remove("__tag__")
          let constr = value-tag.split("/").last()
          if not spec.constrs.keys().contains(constr) {
            error = fail-val(
              err(
                "unknown constructor `"
                  + value-tag
                  + "`, expected "
                  + spec
                    .constrs
                    .keys()
                    .map(k => "`" + k + "`")
                    .join(", ", last: " or "),
              ),
              spec,
              value,
              top.wrap,
            )
          } else {
            let constr-spec = spec.constrs.at(constr)
            if (
              std.type(constr-spec) != std.dictionary
                or not constr-spec.keys().contains("__tag__")
                or not constr-spec.__tag__.starts-with("constr-spec/")
            ) {
              constr-spec = result-unwrap(constr-spec-parse(constr-spec))
            }
            let constr-name = if spec.name == auto {
              constr
            } else {
              str(spec.name).split("(").first() + "-" + constr
            }
            if constr-spec.__tag__ == "constr-spec/null" {
              if fields-value.len() > 0 {
                error = err(
                  "expected no arguments",
                  trace: (
                    __tag__: "trace/constr-null",
                    constr-spec: constr-spec,
                    value: arguments(..fields-value),
                  ),
                )
                error = trace-wrap(
                  (__tag__: "trace/constr", name: constr-name),
                  error,
                )
                error = fail-val(error, spec, value, top.wrap)
              } else {
                results.push((__tag__: value-tag))
              }
            } else if constr-spec.__tag__ == "constr-spec/fields" {
              let named = fields-value
              let keys = ()
              let plan = ()
              let missing = none
              for (field-name, field-spec) in constr-spec.fields.pairs() {
                if named.keys().contains(field-name) {
                  let arg = named.remove(field-name)
                  keys.push(field-name)
                  plan.push((
                    k: "val",
                    spec: field-spec,
                    value: arg,
                    wrap: ((
                      __tag__: "trace/constr-field",
                      constr-spec: constr-spec,
                      constr-arg: field-name,
                      value: arg,
                    ),),
                  ))
                } else {
                  missing = field-name
                  break
                }
              }
              tasks.push((
                k: "build-constr",
                kind: "enum",
                cname: constr-name,
                out-tag: value-tag,
                cspec: constr-spec,
                keys: keys,
                extra: named,
                spec: spec,
                value: value,
                wrap: top.wrap,
              ))
              if missing != none {
                tasks.push((
                  k: "fail-missing",
                  cspec: constr-spec,
                  fname: missing,
                ))
              }
              for item in plan.rev() {
                tasks.push(item)
              }
            } else {
              panic(
                "unknown constructor spec kind: `" + constr-spec.__tag__ + "`",
              )
            }
          }
        }
      } else if tag == "spec/struct" {
        if std.type(value) != std.dictionary {
          error = fail-val(
            err("expected dictionary, got `" + str(std.type(value)) + "`"),
            spec,
            value,
            top.wrap,
          )
        } else {
          let constr-spec = (
            __tag__: "constr-spec/fields",
            fields: spec.fields,
          )
          let named = value
          let keys = ()
          let plan = ()
          let missing = none
          for (field-name, field-spec) in spec.fields.pairs() {
            if named.keys().contains(field-name) {
              let arg = named.remove(field-name)
              keys.push(field-name)
              plan.push((
                k: "val",
                spec: field-spec,
                value: arg,
                wrap: ((
                  __tag__: "trace/constr-field",
                  constr-spec: constr-spec,
                  constr-arg: field-name,
                  value: arg,
                ),),
              ))
            } else {
              missing = field-name
              break
            }
          }
          tasks.push((
            k: "build-constr",
            kind: "struct",
            cname: none,
            out-tag: none,
            cspec: constr-spec,
            keys: keys,
            extra: named,
            spec: spec,
            value: value,
            wrap: top.wrap,
          ))
          if missing != none {
            tasks.push((
              k: "fail-missing",
              cspec: constr-spec,
              fname: missing,
            ))
          }
          for item in plan.rev() {
            tasks.push(item)
          }
        }
      } else if tag == "spec/fix" {
        let unfold = spec.at("fun")
        tasks.push((
          k: "val",
          spec: unfold(spec),
          value: value,
          wrap: (
            ((__tag__: "trace/val", spec: spec, value: value),)
              + top.wrap
          ),
        ))
      } else if tag == "spec/self" {
        panic("cannot validate an unbound recursive self spec")
      } else {
        panic("unknown spec kind: `" + tag + "`")
      }
    } else if top.k == "fail-missing" {
      error = err(
        "not enough arguments",
        trace: (
          __tag__: "trace/constr-missing-arg",
          constr-spec: top.cspec,
          constr-arg: top.fname,
        ),
      )
    } else if top.k == "build-array" {
      let vals = results.slice(results.len() - top.count)
      results = results.slice(0, results.len() - top.count)
      results.push(vals)
    } else if top.k == "build-dict" {
      let vals = results.slice(results.len() - top.keys.len())
      results = results.slice(0, results.len() - top.keys.len())
      let out = (:)
      for (i, key) in top.keys.enumerate() {
        out.insert(key, vals.at(i))
      }
      results.push(out)
    } else if top.k == "build-constr" {
      if top.extra.len() > 0 {
        let extra-args = arguments(..top.extra)
        error = err(
          "unrecognized arguments: " + repr(extra-args),
          trace: (
            __tag__: "trace/constr-extra-args",
            constr-spec: top.cspec,
            extra-args: extra-args,
          ),
        )
        if top.kind == "enum" {
          error = trace-wrap((__tag__: "trace/constr", name: top.cname), error)
        }
        error = fail-val(error, top.spec, top.value, top.wrap)
      } else {
        let vals = results.slice(results.len() - top.keys.len())
        results = results.slice(0, results.len() - top.keys.len())
        let fields = (:)
        for (i, key) in top.keys.enumerate() {
          fields.insert(key, vals.at(i))
        }
        results.push(
          if top.kind == "enum" {
            (__tag__: top.out-tag, ..fields)
          } else {
            fields
          },
        )
      }
    } else if top.k == "union" {
      // The active member succeeded: its value is already on the result
      // stack, so the choice point simply disappears.
    } else {
      panic("invalid validation task: `" + repr(top) + "`")
    }
  }

  if error != none {
    error
  } else {
    assert(
      results.len() == 1,
      message: "validation machine finished with "
        + str(results.len())
        + " results",
    )
    ok(results.first())
  }
}

/// Validates function call arguments with the default validator.
///
/// Returns validated Typst `arguments`.
/// -> function
#let validate-args = validate-args-aux.with(validate)

/// Validates constructor arguments with the default validator.
///
/// Returns a dictionary of validated fields.
/// -> function
#let validate-constr = validate-constr-aux.with(validate)
