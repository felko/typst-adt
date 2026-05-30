#import "../bootstrap.typ": *
#import "../spec.typ": fun, struct
#import "../validate.typ": *
#import "../std.typ" as std
#import "common.typ": *

/// Builds a validated function application eliminator.
/// -> function
#let generate-function-elim(dom, cod) = value => (..args) => {
  let value = pretty-result-unwrap(validate(
    (__tag__: "spec/function", name: auto, dom: dom, cod: cod),
    value,
  ))
  let args = pretty-result-unwrap(validate-args(dom, ..args))
  pretty-result-unwrap(validate(cod, value(..args)))
}

/// Converts a constructor spec to function arguments.
/// -> arguments
#let constr-spec-args(constr-spec) = constr-spec-elim(
  null: arguments(),
  fields: fields => arguments(..fields),
)(constr-spec)

/// Builds the expected case-function spec for an eliminator.
///
/// `T` is the result spec shared by every case function.
/// -> spec
#let CASES(spec, T) = struct(
  ..spec-elim(
    empty_case: () => panic("empty specs do not have cases"),
    builtin: type_ => panic("builtin specs do not have cases"),
    any: () => panic("any specs do not have cases"),
    union_case: (name, elems) => panic("union specs do not have cases"),
    enum: (name, constrs) => constrs
      .pairs()
      .map(((constr-name, constr-spec)) => (
        constr-name,
        fun(..constr-spec-args(constr-spec))(T),
      ))
      .to-dict(),
    struct: (name, fields) => (mk: fun(..fields)(T)),
    array: (name, inner) => panic("array specs do not have cases"),
    dict: (name, value) => panic(
      "dictionary specs do not have cases",
    ),
    function: (name, dom, cod) => panic(
      "function specs do not have cases",
    ),
    fix: (name, fun) => CASES(fun(spec), T),
    self: depth => panic("self specs do not have cases"),
  )(spec),
)

/// Builds a struct eliminator from its single case.
/// -> function
#let elim-struct(spec, mk) = value => {
  let fields = pretty-result-unwrap(validate(spec, value))
  if std.type(mk) == std.function {
    mk(..fields.values())
  } else {
    mk
  }
}

/// Builds an `any` eliminator from its single case.
/// -> function
#let elim-any(f) = x => f(x)

/// Eliminates a value directly from a spec.
///
/// Enum specs require one named case per constructor.
/// -> function
#let elim(spec, ..cases) = spec-elim(
  builtin: type_ => generate-value-elim(spec)(..cases),
  struct: (name, fields) => elim-struct(spec, ..cases),
  any: () => elim-any(..cases),
  union: (name, elems) => generate-value-elim(spec)(..cases),
  enum: (name, constrs) => {
    assert(
      cases.pos().len() == 0,
      message: "expected no positional arguments, got " + repr(cases.pos()),
    )
    let named-cases = cases.named()
    assert(
      named-cases.keys().all(k => constrs.keys().contains(k)),
      message: "unrecognized cases: "
        + named-cases
          .keys()
          .filter(k => not constrs.keys().contains(k))
          .map(k => "`" + k + "`")
          .join(", "),
    )
    assert(
      constrs.keys().all(k => named-cases.keys().contains(k)),
      message: "missing cases: "
        + constrs
          .keys()
          .filter(k => not named-cases.keys().contains(k))
          .map(k => "`" + k + "`")
          .join(", "),
    )
    value => {
      if (
        std.type(value) == std.dictionary and value.keys().contains("__tag__")
      ) {
        let tag = value.remove("__tag__").split("/").last()
        let args = pretty-result-unwrap(project-constr(
          spec.constrs.at(tag),
          value,
        ))
        let case = named-cases.at(tag)
        if std.type(case) == std.function {
          case(..args.values())
        } else {
          case
        }
      } else {
        panic("not an enum value", value)
      }
    }
  },
  array: (name, inner) => generate-value-elim(spec)(..cases),
  dictionary: (name, inner) => generate-value-elim(spec)(..cases),
  function: (name, dom, cod) => generate-function-elim(dom, cod)(..cases),
  fix: (name, fun) => elim(fun(spec), ..cases),
  self: (..args) => panic("self specs do not support direct eliminators"),
)(spec)

/// Generates eliminators for a spec.
///
/// Enum eliminators require one case per constructor.
/// -> dictionary
#let generate-elim(spec) = spec-elim(
  self: (..args) => (:),
  __default__: (..args) => (elim: elim.with(spec)),
)(spec)
