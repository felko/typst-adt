#import "../bootstrap.typ": *
#import "../validate.typ": *
#import "../std.typ" as std
#import "common.typ": *

/// Wraps a function with argument and return validation.
/// -> function
#let generate-function-intro(dom, cod) = f => {
  assert(
    std.type(f) == std.function,
    message: "expected function, got `" + str(std.type(f)) + "`",
  )
  (..args) => {
    let args = pretty-result-unwrap(validate-args(dom, ..args))
    pretty-result-unwrap(validate(cod, f(..args)))
  }
}

/// Builds constructor functions for every enum constructor.
/// -> dictionary(str, function)
#let generate-enum-intros(spec, constrs) = (
  constrs
    .pairs()
    .map(((constr-name, constr-spec)) => (
      constr-name,
      generate-constr-with-spec(spec, constr-name, constr-spec),
    ))
    .to-dict()
)

/// Returns constructor-specific intros for an enum spec.
/// -> dictionary(str, function)
#let intro-enum(spec, constrs) = generate-enum-intros(spec, constrs)

/// Introduces a value directly from a spec.
///
/// Enum specs return a dictionary containing one intro per constructor.
/// -> any
#let intro(spec, ..args) = spec-elim(
  builtin: type_ => generate-value-intro(spec)(..args),
  any: () => generate-value-intro(spec)(..args),
  union: (name, elems) => generate-value-intro(spec)(..args),
  struct: (name, fields) => generate-constr-with-spec(
    spec,
    none,
    (__tag__: "constr-spec/fields", fields: fields),
  )(..args),
  enum: (name, constrs) => intro-enum(spec, constrs, ..args),
  array: (name, inner) => generate-value-intro(spec)(..args),
  dictionary: (name, inner) => generate-value-intro(spec)(..args),
  function: (name, dom, cod) => generate-function-intro(dom, cod)(..args),
  fix: (name, fun) => spec-elim(
    struct: (name, fields) => generate-constr-with-spec(
      spec,
      none,
      (__tag__: "constr-spec/fields", fields: fields),
    )(..args),
    enum: (name, constrs) => intro-enum(spec, constrs, ..args),
    __default__: (..args) => panic("spec does not support direct intros"),
  )(fun(spec)),
  self: (..args) => panic("self specs do not support direct intros"),
)(spec)

/// Generates intro helpers for a spec.
///
/// Returns `intro` for most specs and both `intro`/`intros` for enums.
/// -> dictionary
#let generate-intro(spec) = spec-elim(
  builtin: type_ => (intro: intro.with(spec)),
  any: () => (intro: intro.with(spec)),
  union: (name, elems) => (intro: intro.with(spec)),
  struct: (name, fields) => (intro: intro.with(spec)),
  enum: (name, constrs) => {
    let intros = generate-enum-intros(spec, constrs)
    (intro: intros, intros: intros)
  },
  array: (name, inner) => (intro: intro.with(spec)),
  dictionary: (name, inner) => (intro: intro.with(spec)),
  function: (name, dom, cod) => (intro: intro.with(spec)),
  fix: (name, fun) => spec-elim(
    struct: (name, fields) => (intro: intro.with(spec)),
    enum: (name, constrs) => {
      let intros = generate-enum-intros(spec, constrs)
      (intro: intros, intros: intros)
    },
    __default__: (..args) => (:),
  )(fun(spec)),
  self: (..args) => (:),
)(spec)
