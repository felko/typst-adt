#import "../bootstrap.typ": *
#import "../validate.typ": *

/// Returns whether a spec is a fixed-point spec.
/// -> bool
#let spec-is-fix(spec) = spec-elim(
  empty_case: () => false,
  builtin: type_ => false,
  any: () => false,
  union_case: (name, elems) => false,
  enum: (name, constrs) => false,
  struct: (name, fields) => false,
  array_case: (name, inner) => false,
  dictionary_case: (name, key, value) => false,
  function_case: (name, dom, cod) => false,
  fix: (name, fun) => true,
  self: depth => false,
)(spec)

/// Projects constructor arguments into validated fields.
///
/// Recursive fields are left as-is so recursive values can be projected without
/// forcing validation of the whole recursive shape.
/// -> RESULT(dictionary)
#let project-constr-args(constr-spec, ..args) = constr-spec-elim(
  none_: {
    let named = args.named()
    if args.pos().len() != 0 or named.len() != 0 {
      err("expected no arguments, got `" + repr(args) + "`")
    } else {
      ok((:))
    }
  },
  fields: field-specs => {
    let (pos, named) = (args.pos(), args.named())
    let ann = if named.keys().contains("__ann__") {
      let ann = named.remove("__ann__")
      assert(
        type(ann) == dictionary,
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
        return err("not enough arguments: `" + repr(args) + "`")
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
          return result
        }
        fields.insert(field-name, result.value)
      }
    }
    for (ann-name, ann-value) in ann.pairs() {
      fields.insert(ann-name, ann-value)
    }
    if pos.len() > 0 or named.len() > 0 {
      err("unrecognizd arguments: `" + repr(arguments(..pos, ..named)) + "`")
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
  none_: if tag == none { (:) } else { (__tag__: tag) },
  fields: _ => {
    if tag == none {
      (..args) => result-unwrap(validate-constr(constr-spec, ..args))
    } else {
      (..args) => (
        __tag__: tag,
        ..result-unwrap(validate-constr(constr-spec, ..args)),
      )
    }
  },
)(constr-spec)

/// Builds a constructor for a generated spec.
///
/// Generated constructors return plain values and do not attach methods.
/// -> function | dictionary
#let generate-constr-with-spec(tag, constr-spec) = constr-spec-elim(
  none_: if tag == none { (:) } else { (__tag__: tag) },
  fields: _ => {
    if tag == none {
      (..args) => result-unwrap(project-constr-args(constr-spec, ..args))
    } else {
      (..args) => (
        __tag__: tag,
        ..result-unwrap(project-constr-args(constr-spec, ..args)),
      )
    }
  },
)(constr-spec)

/// Builds an intro function for plain validated values.
/// -> function
#let generate-value-intro(spec) = value => result-unwrap(validate(spec, value))

/// Builds an eliminator for plain validated values.
/// -> function
#let generate-value-elim(spec) = f => value => {
  let value = result-unwrap(validate(spec, value))
  if type(f) == function {
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
  none_: ok((:)),
  fields: field-specs => {
    let fields = (:)
    for (field-name, field-spec) in field-specs.pairs() {
      if not value.keys().contains(field-name) {
        return err("missing field `" + field-name + "`")
      }
      if spec-is-fix(field-spec) {
        fields.insert(field-name, value.at(field-name))
      } else {
        let result = validate(field-spec, value.at(field-name))
        if result-is-err(result) {
          return result
        }
        fields.insert(field-name, result.value)
      }
    }
    ok(fields)
  },
)(constr-spec)
