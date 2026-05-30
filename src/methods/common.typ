#import "../bootstrap.typ": *
#import "../validate.typ": *
#import "../std.typ" as std

/// Returns whether a spec is a fixed-point spec.
/// -> bool
#let spec-is-fix(spec) = spec-elim(
  fix: (name, fun) => true,
  __default__: false,
)(spec)

/// Builds a readable generated-constructor name for traces.
/// -> str
#let constr-trace-name(spec, tag) = {
  let name = spec.at("name", default: auto)
  if name == auto {
    tag
  } else {
    str(name).split("(").first() + "-" + tag
  }
}

/// Adds generated-constructor context to a result.
/// -> RESULT(any)
#let result-trace-constr(spec, tag, result) = {
  if tag == none {
    result
  } else {
    result-trace(
      cont => (
        __tag__: "trace/constr",
        name: constr-trace-name(spec, tag),
        cont: cont,
      ),
      result,
    )
  }
}

/// Projects constructor arguments into validated fields.
///
/// Recursive fields are left as-is so recursive values can be projected without
/// forcing validation of the whole recursive shape.
/// -> RESULT(dictionary)
#let project-constr-args(constr-spec, ..args) = constr-spec-elim(
  null: {
    let named = args.named()
    if args.pos().len() != 0 or named.len() != 0 {
      err(
        "expected no arguments",
        trace: (
          __tag__: "trace/constr-null",
          constr-spec: constr-spec,
          value: args,
        ),
      )
    } else {
      ok((:))
    }
  },
  fields: field-specs => {
    let (pos, named) = (args.pos(), args.named())
    let ann = if named.keys().contains("__ann__") {
      let ann = named.remove("__ann__")
      assert(
        std.type(ann) == std.dictionary,
        message: "expected `__ann__` to be a dictionary",
      )
      ann
    } else {
      (:)
    }
    let fields = (:)
    for (field-name, field-spec) in field-specs.pairs() {
      let arg = if named.keys().contains(field-name) {
        named.remove(field-name)
      } else if ann.keys().contains(field-name) {
        ann.remove(field-name)
      } else if pos.len() == 0 {
        return err(
          "not enough arguments",
          trace: (
            __tag__: "trace/constr-missing-arg",
            constr-spec: constr-spec,
            constr-arg: field-name,
          ),
        )
      } else {
        let (arg, ..new-pos) = pos
        pos = new-pos
        arg
      }
      if spec-is-fix(field-spec) {
        fields.insert(field-name, arg)
      } else {
        let result = validate(field-spec, arg)
        if result-is-err(result) {
          return result-trace(
            cont => (
              __tag__: "trace/constr-field",
              constr-spec: constr-spec,
              constr-arg: field-name,
              value: arg,
              cont: cont,
            ),
            result,
          )
        }
        fields.insert(field-name, result.value)
      }
    }
    for (ann-name, ann-value) in ann.pairs() {
      fields.insert(ann-name, ann-value)
    }
    if pos.len() > 0 or named.len() > 0 {
      let extra-args = arguments(..pos, ..named)
      err(
        "unrecognized arguments: " + repr(extra-args),
        trace: (
          __tag__: "trace/constr-extra-args",
          constr-spec: constr-spec,
          extra-args: extra-args,
        ),
      )
    } else {
      ok(fields)
    }
  },
)(constr-spec)

/// Builds a raw constructor for a constructor spec.
///
/// Prefer generated constructors from `generate` for user-facing values.
/// -> function | dictionary
#let generate-constr(tag, constr-spec) = constr-spec-elim(
  null: if tag == none { (:) } else { (__tag__: tag) },
  fields: _ => {
    if tag == none {
      (..args) => pretty-result-unwrap(validate-constr(constr-spec, ..args))
    } else {
      (..args) => (
        __tag__: tag,
        ..pretty-result-unwrap(result-trace(
          cont => (__tag__: "trace/constr", name: tag, cont: cont),
          validate-constr(constr-spec, ..args),
        )),
      )
    }
  },
)(constr-spec)

/// Builds a constructor for a generated spec.
///
/// Generated constructors return plain values and do not attach methods.
/// -> function | dictionary
#let generate-constr-with-spec(spec, tag, constr-spec) = constr-spec-elim(
  null: if tag == none { (:) } else { (__tag__: tag) },
  fields: _ => {
    if tag == none {
      (..args) => pretty-result-unwrap(project-constr-args(constr-spec, ..args))
    } else {
      (..args) => (
        __tag__: tag,
        ..pretty-result-unwrap(result-trace-constr(
          spec,
          tag,
          project-constr-args(constr-spec, ..args),
        )),
      )
    }
  },
)(constr-spec)

/// Builds an intro function for plain validated values.
/// -> function
#let generate-value-intro(spec) = value => pretty-result-unwrap(validate(spec, value))

/// Builds an eliminator for plain validated values.
/// -> function
#let generate-value-elim(spec) = f => value => {
  let value = pretty-result-unwrap(validate(spec, value))
  if std.type(f) == std.function {
    f(value)
  } else {
    f
  }
}

/// Projects validated fields out of a constructor value.
///
/// Extra fields are ignored.
/// -> RESULT(dictionary)
#let project-constr(constr-spec, value) = constr-spec-elim(
  null: ok((:)),
  fields: field-specs => {
    let fields = (:)
    for (field-name, field-spec) in field-specs.pairs() {
      if not value.keys().contains(field-name) {
        return err(
          "missing field `" + field-name + "`",
          trace: (
            __tag__: "trace/constr-missing-field",
            constr-spec: constr-spec,
            constr-arg: field-name,
          ),
        )
      }
      if spec-is-fix(field-spec) {
        fields.insert(field-name, value.at(field-name))
      } else {
        let result = validate(field-spec, value.at(field-name))
        if result-is-err(result) {
          return result-trace(
            cont => (
              __tag__: "trace/constr-field",
              constr-spec: constr-spec,
              constr-arg: field-name,
              value: value.at(field-name),
              cont: cont,
            ),
            result,
          )
        }
        fields.insert(field-name, result.value)
      }
    }
    ok(fields)
  },
)(constr-spec)
