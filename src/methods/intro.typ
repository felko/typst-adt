#import "../bootstrap.typ": *
#import "../validate.typ": *
#import "common.typ": *

/// Wraps a function with argument and return validation.
/// -> function
#let generate-function-intro(dom, cod) = f => {
  assert(
    type(f) == function,
    message: "expected function, got `" + str(type(f)) + "`",
  )
  (..args) => {
    let args = result-unwrap(validate-args(dom, ..args))
    result-unwrap(validate(cod, f(..args)))
  }
}

/// Builds constructor functions for every enum constructor.
/// -> dictionary(str, function)
#let generate-enum-intros(spec, constrs) = (
  constrs
    .pairs()
    .map(((constr-name, constr-spec)) => (
      constr-name,
      generate-constr-with-spec(constr-name, constr-spec),
    ))
    .to-dict()
)

/// Generates intro helpers for a spec.
///
/// Returns `intro` for most specs and both `intro`/`intros` for enums.
/// -> dictionary
#let generate-intro(spec) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (intro: generate-value-intro(spec)),
  any: () => (intro: value => value),
  union_case: (name, elems) => (intro: generate-value-intro(spec)),
  struct: (name, fields) => (
    intro: generate-constr-with-spec(
      none,
      (__tag__: "constr-spec/fields", fields: fields),
    ),
  ),
  enum: (name, constrs) => {
    let intros = generate-enum-intros(spec, constrs)
    (intro: intros, intros: intros)
  },
  array_case: (name, inner) => (intro: generate-value-intro(spec)),
  dictionary_case: (name, key, inner) => (intro: generate-value-intro(spec)),
  function_case: (name, dom, cod) => (intro: generate-function-intro(dom, cod)),
  fix: (name, fun) => spec-elim(
    empty_case: () => (:),
    builtin: type_ => (:),
    any: () => (:),
    union_case: (name, elems) => (:),
    struct: (name, fields) => (
      intro: generate-constr-with-spec(
        none,
        (__tag__: "constr-spec/fields", fields: fields),
      ),
    ),
    enum: (name, constrs) => {
      let intros = generate-enum-intros(spec, constrs)
      (intro: intros, intros: intros)
    },
    array_case: (name, inner) => (:),
    dictionary_case: (name, key, value) => (:),
    function_case: (name, dom, cod) => (:),
    fix: (name, fun) => (:),
    self: (..args) => (:),
  )(fun(spec)),
  self: (..args) => (:),
)(spec)

