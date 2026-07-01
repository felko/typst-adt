#import "../bootstrap.typ": *
#import "../validate.typ": *
#import "../std.typ" as std
#import "common.typ": *

#let constr-repr(constr-spec, data) = constr-spec-elim(
  null: "",
  fields: fields => {
    (
      "("
        + fields
          .pairs()
          .map(((field-name, field-spec)) => {
            repr(field-spec, data.at(field-name))
          })
          .join(", ")
        + ")"
    )
  },
)

/// Generates tag checks for (possibly recursive) enums.
/// -> dictionary
#let repr(spec, value) = spec-elim(
  builtin: type_ => std.repr(value),
  any: () => std.repr(value),
  union: (name, elems) => {
    for elem in elems {
      if result-is-ok(validate(elem, value)) {
        return repr(elem, value)
      }
    }
    panic(
      "value "
        + std.repr(value)
        + " does not inhabit of any of "
        + elems.map(t => "`" + to-string(t) + "`").join(", ", last: " or "),
    )
  },
  struct: (name, fields) => (
    (if name == auto { "" } else { name + " " })
      + "{ "
      + fields
        .pairs()
        .map(((field-name, field-spec)) => {
          field-name + ": " + repr(field-spec, value.at(field-name))
        })
        .join(", ")
      + " }"
  ),
  enum: (name, constrs) => {
    let tag = value.remove("__tag__")
    let constr-spec = constrs.at(tag)
    (
      (if name == auto { "" } else { name + "/" })
        + tag
        + constr-repr(constr-spec, value)
    )
  },
  array: (name, inner) => std.repr(value),
  dictionary: (name, inner) => std.repr(value),
  function: (name, dom, cod) => std.repr(value),
  fix: (name, fun) => repr(fun(spec), value),
  self: (..args) => std.repr(value),
)(spec)
