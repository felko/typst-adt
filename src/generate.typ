#import "bootstrap.typ": *
#import "validate.typ": *

#let generate-constr(tag, constr-spec) = constr-spec-elim(
  none_: if tag == none { (:) } else { (__tag__: tag) },
  fields: _ => {
    if tag == none {
      (.. args) => result-unwrap(validate-constr(constr-spec, .. args))
    } else {
      (.. args) => (
        __tag__: tag,
        .. result-unwrap(validate-constr(constr-spec, .. args))
      )
    }
  }
)(constr-spec)

#let generate-intro(spec) = spec-elim(
  builtin: type_ => (:),
  any: () => (intro: x => x),
  struct: fields => (
    intro: generate-constr(none, constr-spec-fields(spec.fields))
  ),
  enum: (name, constrs) => (
    intros: constrs.pairs().map(((constr-name, constr-spec)) => {
      (constr-name, generate-constr(constr-name, constr-spec))
    }).to-dict()
  ),
  fix: (name, fun) => generate-intro(fun(spec)),
  self: (.. args) => (:),
)(spec)

#let CASES(spec, T) = spec-struct(
  .. if spec.__tag__ == "spec/enum" {
    spec.constrs.pairs().map(((constr-name, constr-spec)) => {
      (constr-name, spec-function(.. constr-spec-args(constr-spec))(T))
    }).to-dict()
  } else if spec.__tag__ == "spec/struct" {
    (mk: spec-function(.. spec.fields)(T))
  } else {
    panic("todo")
  }
)

#let generate-elim(spec) = spec-elim(
  builtin: type_ => (:),
  struct: (name, fields) => (:),
  any: () => (elim: f => x => f(x)),
  enum: (name, constrs) => (elim: (.. cases) => {
    assert(
      cases.pos().len() == 0,
      message: "expected no positional arguments, got " + repr(cases.pos())
    )
    cases = cases.named()
    assert(
      cases.keys().all(k => constrs.keys().contains(k)),
      message: "unrecognized cases: " + cases.keys().filter(k => not constrs.keys().contains(k)).map(k => "`" + k + "`").join(", ")
    )
    assert(
      constrs.keys().all(k => cases.keys().contains(k)),
      message: "missing cases: " + constrs.keys().filter(k => not cases.keys().contains(k)).map(k => "`" + k + "`").join(", ")
    )
    value => {
      if type(value) == dictionary and value.keys().contains("__tag__") {
        let tag = value.remove("__tag__").split("/").last()
        let args = result-unwrap(validate-constr(spec.constrs.at(tag), .. value))
        let case = cases.at(tag)
        if type(case) == function {
          case(.. args.values())
        } else {
          case
        }
      } else {
        panic("not an enum value: `" + repr(value) + "`")
      }
    }
  }),
  fix: (name, fun) => generate-elim(fun(spec)),
  self: (.. args) => (:),
)(spec)

#let generate-fields(spec) = spec-elim(
  builtin: type_ => (:),
  any: () => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => (:),
  fix: (name, fun) => (:),
  self: (.. args) => (:),
)(spec)

#let generate-rec(spec) = spec-elim(
  builtin: type_ => (:),
  any: () => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => (:),
  fix: (name, fun) => (rec: (.. cases) => {
    assert(
      cases.pos().len() == 0,
      message: "expected no positional arguments, got " + repr(cases.pos())
    )
    cases = cases.named()
    assert(
      cases.keys().all(k => constrs.keys().contains(k)),
      message: "unrecognized cases: " + cases.keys().filter(k => not constrs.keys().contains(k)).map(k => "`" + k + "`").join(", ")
    )
    assert(
      constrs.keys().all(k => cases.keys().contains(k)),
      message: "missing cases: " + constrs.keys().filter(k => not cases.keys().contains(k)).map(k => "`" + k + "`").join(", ")
    )
    value => {
      if type(value) == dictionary and value.keys().contains("__tag__") {
        let tag = value.remove("__tag__").split("/").last()
        let args = result-unwrap(validate-constr(spec.constrs.at(tag), .. value))
        let case = cases.at(tag)
        if type(case) == function {
          case(.. args.values())
        } else {
          case
        }
      } else {
        panic("not an enum value: `" + repr(value) + "`")
      }
    }
  }),
  self: (.. args) => (:),
)(spec)

#let generate-visit(spec) = spec-elim(
  builtin: type_ => (:),
  any: () => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => (:),
  fix: (name, fun) => (:),
  self: (.. args) => (:),
)(spec)

#let generate(spec) = (:
  .. generate-intro(spec),
  .. generate-elim(spec),
  .. generate-fields(spec),
  .. generate-rec(spec),
  .. generate-visit(spec),
)