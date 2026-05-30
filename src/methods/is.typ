#import "../bootstrap.typ": *
#import "../validate.typ": *
#import "../std.typ" as std
#import "common.typ": *

/// Builds a function checking whether an enum value has
/// a given tag.
///
/// Panics when the current constructor does not provide the requested field.
/// -> function
#let generate-enum-is(spec, expected-tag) = value => {
  if std.type(value) != std.dictionary or not value.keys().contains("__tag__") {
    panic("not an enum value", value)
  }
  let tag = value.remove("__tag__").split("/").last()
  if not constrs.keys().contains(tag) {
    panic("unknown constructor `" + tag + "`")
  }
  return tag == expected-tag
}

/// Builds tag checker functions for a given enum spec.
/// -> dictionary
#let generate-enum-iss(spec) = {
  let tag-names = spec.constrs.keys()
  (
    is: tag-names
      .map(tag-name => (
        tag-name,
        generate-enum-is(spec, tag-name),
      ))
      .to-dict(),
  )
}

/// Generates tag checks for (possibly recursive) enums.
/// -> dictionary
#let generate-is(spec) = spec-elim(
  enum: (name, constrs) => generate-enum-iss(spec),
  fix: (name, fun) => generate-enum-iss(fun(spec)),
  __default__: (..args) => (:),
)(spec)
