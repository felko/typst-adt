#import "../bootstrap.typ": *
#import "../spec.typ": fun, struct
#import "../validate.typ": *
#import "common.typ": *

/// Builds a validated function application eliminator.
/// -> function
#let generate-function-elim(dom, cod) = value => (..args) => {
  let value = result-unwrap(validate(
    (__tag__: "spec/function", name: auto, dom: dom, cod: cod),
    value,
  ))
  let args = result-unwrap(validate-args(dom, ..args))
  result-unwrap(validate(cod, value(..args)))
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

/// Generates eliminators for a spec.
///
/// Enum eliminators require one case per constructor.
/// -> dictionary
#let generate-elim(spec) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (elim: generate-value-elim(spec)),
  struct: (name, fields) => (
    elim: mk => value => {
      let fields = result-unwrap(validate(spec, value))
      if type(mk) == function {
        mk(..fields.values())
      } else {
        mk
      }
    },
  ),
  any: () => (elim: f => x => f(x)),
  union_case: (name, elems) => (elim: generate-value-elim(spec)),
  enum: (name, constrs) => (
    elim: (..cases) => {
      assert(
        cases.pos().len() == 0,
        message: "expected no positional arguments, got " + repr(cases.pos()),
      )
      cases = cases.named()
      assert(
        cases.keys().all(k => constrs.keys().contains(k)),
        message: "unrecognized cases: "
          + cases
            .keys()
            .filter(k => not constrs.keys().contains(k))
            .map(k => "`" + k + "`")
            .join(", "),
      )
      assert(
        constrs.keys().all(k => cases.keys().contains(k)),
        message: "missing cases: "
          + constrs
            .keys()
            .filter(k => not cases.keys().contains(k))
            .map(k => "`" + k + "`")
            .join(", "),
      )
      value => {
        if type(value) == dictionary and value.keys().contains("__tag__") {
          let tag = value.remove("__tag__").split("/").last()
          let args = result-unwrap(project-constr(spec.constrs.at(tag), value))
          let case = cases.at(tag)
          if type(case) == function {
            case(..args.values())
          } else {
            case
          }
        } else {
          panic("not an enum value: `" + repr(value) + "`")
        }
      }
    },
  ),
  array: (name, inner) => (elim: generate-value-elim(spec)),
  dict: (name, inner) => (elim: generate-value-elim(spec)),
  function: (name, dom, cod) => (elim: generate-function-elim(dom, cod)),
  fix: (name, fun) => generate-elim(fun(spec)),
  self: (..args) => (:),
)(spec)

