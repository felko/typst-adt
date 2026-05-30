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
    for (index, (pos-spec, pos-value)) in args-spec.pos().zip(args.pos()).enumerate() {
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
        "unexpected argument"
          + plural
          + ": "
          + repr(named-args.keys()),
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
/// -> RESULT(any)
#let validate(
  /// Spec to validate against.
  /// -> spec
  spec,
  /// Value to validate.
  /// -> any
  value,
) = {
  let normalized-spec = if std.type(spec) == std.type {
    result-unwrap(spec-parse(spec))
  } else {
    spec
  }
  let result = spec-elim(
  builtin: type_ => {
    assert(std.type(type_) == std.type)
    if std.type(value) == type_ {
      ok(value)
    } else {
      err(
        "expected a value of type `"
          + str(type_)
          + "`, got `"
          + str(std.type(value))
          + "`",
      )
    }
  },
  any: () => {
    ok(value)
  },
  enum: (name, constrs) => {
    let value = value
    if std.type(value) == std.dictionary and value.keys().contains("__tag__") {
      let tag = value.remove("__tag__")
      let constr = tag.split("/").last()
      if constrs.keys().contains(constr) {
        let constr-spec = constrs.at(constr)
        result-map(
          fields => (__tag__: tag, ..fields),
          result-trace(
            cont => (
              __tag__: "trace/constr",
              name: if name == auto {
                constr
              } else {
                str(name).split("(").first() + "-" + constr
              },
              cont: cont,
            ),
            validate-constr-aux(
              validate,
              constr-spec,
              ..value,
            ),
          ),
        )
      } else {
        err(
          "unknown constructor `"
            + tag
            + "`, expected "
            + constrs.keys().map(k => "`" + k + "`").join(", ", last: " or "),
        )
      }
    } else {
      err("not an enum value: " + repr(value))
    }
  },
  union: (name, elems) => {
    let first-error = none
    for elem in elems {
      let result = validate(elem, value)
      if result-is-ok(result) {
        return result
      } else if first-error == none {
        first-error = result-trace(
          cont => (
            __tag__: "trace/union",
            spec: elem,
            value: value,
            cont: cont,
          ),
          result,
        )
      }
    }
    if first-error == none {
      err("empty type has no values")
    } else {
      first-error
    }
  },
  struct: (name, fields-spec) => {
    if std.type(value) != std.dictionary {
      err("expected dictionary, got `" + str(std.type(value)) + "`")
    } else {
      validate-constr-aux(
        validate,
        (
          __tag__: "constr-spec/fields",
          fields: fields-spec,
        ),
        ..value,
      )
    }
  },
  array: (name, inner) => {
    if std.type(value) != std.array {
      err("expected array, got `" + str(std.type(value)) + "`")
    } else {
      result-all(validate.with(inner), value)
    }
  },
  dictionary: (name, inner) => {
    if std.type(value) != std.dictionary {
      err("expected dictionary, got `" + str(std.type(value)) + "`")
    } else {
      result-all-dict(
        v => validate(inner, v),
        value,
      )
    }
  },
  function: (name, dom, cod) => {
    if std.type(value) == std.function {
      ok(value)
    } else {
      err("expected function, got `" + str(std.type(value)) + "`")
    }
  },
  fix: (name, fun) => validate(fun(normalized-spec), value),
  self: depth => panic("cannot validate an unbound recursive self spec"),
  )(normalized-spec)
  result-trace(
    cont => (
      __tag__: "trace/val",
      spec: normalized-spec,
      value: value,
      cont: cont,
    ),
    result,
  )
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
