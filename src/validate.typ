#import "result.typ": *
#import "spec.typ": *

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
) = args-spec-elim(
  null: {
    if args.pos().len() != 0 or args.named().len() != 0 {
      err("expected no arguments, got `" + repr(args) + "`")
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
      )
    }
    let pos = result-all(
      ((pos-spec, pos-value)) => {
        validate(pos-spec, pos-value)
      },
      args-spec.pos().zip(args.pos()),
    )
    if result-is-err(pos) {
      return pos
    } else {
      pos = pos.value
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
          + named-args.keys().map(k => "`" + k + "`").join(", "),
      )
    } else if missing-named-args.len() > 0 {
      let plural = if missing-named-args.len() > 1 { "s" } else { "" }
      err(
        "missing value"
          + plural
          + "for argument"
          + plural
          + ": "
          + missing-named-args.keys().map(k => "`" + k + "`").join(", "),
      )
    } else {
      result-map(
        named => arguments(..pos, ..named),
        result-all-dict(
          ((arg-spec, arg-value)) => validate(arg-spec, arg-value),
          named-spec-value,
        ),
      )
    }
  },
)(args-spec)

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
) = constr-spec-elim(
  null: {
    let named = args.named()
    if args.pos().len() != 0 or named.len() != 0 {
      err("expected no arguments, got `" + repr(args) + "`")
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
        return err("not enough arguments: `" + repr(args) + "`")
      } else {
        let (arg, ..new-pos) = pos
        pos = new-pos
        arg
      }
      let result = validate(field-spec, arg)
      if result-is-err(result) {
        return result
      } else {
        fields.insert(field-name, result.value)
      }
    }
    if pos.len() > 0 or named.len() > 0 {
      err("unrecognized arguments: `" + repr(arguments(..pos, ..named)) + "`")
    } else {
      ok(fields)
    }
  },
)(constr-spec)

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
) = spec-elim(
  empty_case: () => {
    err("empty type has no values")
  },
  builtin: type_ => {
    assert(type(type_) == type)
    if type(value) == type_ {
      ok(value)
    } else {
      err(
        "expected a value of type `"
          + str(type_)
          + "`, got `"
          + str(type(value))
          + "`",
      )
    }
  },
  any: () => {
    ok(value)
  },
  enum: (name, constrs) => {
    let value = value
    if type(value) == dictionary and value.keys().contains("__tag__") {
      let tag = value.remove("__tag__")
      let constr = tag.split("/").last()
      if constrs.keys().contains(constr) {
        let constr-spec = constrs.at(constr)
        result-map(
          fields => (__tag__: tag, ..fields),
          validate-constr-aux(
            validate,
            constr-spec,
            ..value,
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
      err("not an enum value: `" + repr(value) + "`")
    }
  },
  union_case: (name, elems) => {
    result-any(elem => validate(elem, value), elems)
  },
  struct: (name, fields-spec) => validate-constr-aux(
    validate,
    (
      __tag__: "constr-spec/fields",
      fields: fields-spec,
    ),
    ..value,
  ),
  array: (name, inner) => {
    if type(value) != type(()) {
      err("expected array, got `" + str(type(value)) + "`")
    } else {
      result-all(validate.with(inner), value)
    }
  },
  dict: (name, inner) => {
    if type(value) != dictionary {
      err("expected dictionary, got `" + str(type(value)) + "`")
    } else {
      result-all-dict(
        v => validate(inner, v),
        value,
      )
    }
  },
  function: (name, dom, cod) => {
    if type(value) == function {
      ok(value)
    } else {
      err("expected function, got `" + str(type(value)) + "`")
    }
  },
  fix: (name, fun) => validate(fun(spec), value),
  self: depth => panic("cannot validate an unbound recursive self spec"),
)(spec)

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
