#import "../bootstrap.typ": *
#import "../validate.typ": *
#import "../std.typ" as std
#import "common.typ": *

/// Builds a field accessor for enum values.
///
/// Panics when the current constructor does not provide the requested field.
/// -> function
#let generate-enum-field(spec, constrs, field-name) = value => {
  if std.type(value) != std.dictionary or not value.keys().contains("__tag__") {
    panic("not an enum value", value)
  }
  let tag = value.remove("__tag__").split("/").last()
  if not constrs.keys().contains(tag) {
    panic("unknown constructor `" + tag + "`")
  }
  let constr-spec = constrs.at(tag)
  let fields = pretty-result-unwrap(project-constr(constr-spec, value))
  if fields.keys().contains(field-name) {
    fields.at(field-name)
  } else {
    panic("constructor `" + tag + "` does not have field `" + field-name + "`")
  }
}

/// Builds field accessors shared across enum constructors.
///
/// Accessors are generated for the union of all constructor field names.
/// -> dictionary
#let generate-enum-fields(spec, constrs) = {
  let field-names = ()
  for constr-spec in constrs.values() {
    if constr-spec.__tag__ == "constr-spec/fields" {
      field-names += constr-spec.fields.keys()
    }
  }
  (
    fields: field-names
      .dedup()
      .map(field-name => (
        field-name,
        generate-enum-field(spec, constrs, field-name),
      ))
      .to-dict(),
  )
}

/// Generates field accessors for structs and enums.
/// -> dictionary
#let generate-fields(spec) = spec-elim(
  builtin: type_ => (:),
  any: () => (:),
  union: (name, elems) => (:),
  struct: (name, fields) => (
    fields: fields
      .keys()
      .map(field-name => (
        field-name,
        value => pretty-result-unwrap(validate(spec, value)).at(field-name),
      ))
      .to-dict(),
  ),
  enum: (name, constrs) => generate-enum-fields(spec, constrs),
  array: (name, inner) => (:),
  dictionary: (name, inner) => (:),
  function: (name, dom, cod) => (:),
  fix: (name, fun) => generate-fields(fun(spec)),
  self: (..args) => (:),
)(spec)
