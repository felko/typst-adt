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

#let generate-value-intro(spec) = value => result-unwrap(validate(spec, value))

#let generate-value-elim(spec) = f => value => {
  let value = result-unwrap(validate(spec, value))
  if type(f) == function {
    f(value)
  } else {
    f
  }
}

#let generate-function-intro(dom, cod) = f => {
  assert(type(f) == function, message: "expected function, got `" + str(type(f)) + "`")
  (.. args) => {
    let args = result-unwrap(validate-args(dom, .. args))
    result-unwrap(validate(cod, f(.. args)))
  }
}

#let generate-function-elim(dom, cod) = value => (.. args) => {
  let value = result-unwrap(validate((__tag__: "spec/function", name: auto, dom: dom, cod: cod), value))
  let args = result-unwrap(validate-args(dom, .. args))
  result-unwrap(validate(cod, value(.. args)))
}

#let generate-rec-field(go, field-spec, value) = {
  let is-recursive = spec-elim(
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
  )(field-spec)
  if is-recursive {
    go(value)
  } else {
    value
  }
}

#let generate-enum-rec(spec, constrs) = (rec: (.. cases) => {
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
  let go(value) = {
    value = result-unwrap(validate(spec, value))
    let tag = value.remove("__tag__").split("/").last()
    let constr-spec = constrs.at(tag)
    let fields = result-unwrap(validate-constr(constr-spec, .. value))
    let case = cases.at(tag)
    if type(case) != function {
      return case
    }
    if constr-spec.__tag__ == "constr-spec/none" {
      case()
    } else if constr-spec.__tag__ == "constr-spec/fields" {
      let mapped = (:)
      for (field-name, field-spec) in constr-spec.fields.pairs() {
        mapped.insert(field-name, generate-rec-field(go, field-spec, fields.at(field-name)))
      }
      case(.. mapped.values())
    } else {
      panic("ill-formed constructor spec: `" + repr(constr-spec) + "`")
    }
  }
  go
})

#let generate-enum-intros(constrs) = constrs.pairs().map(((constr-name, constr-spec)) => {
  (constr-name, generate-constr(constr-name, constr-spec))
}).to-dict()

#let generate-intro(spec) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (intro: generate-value-intro(spec)),
  any: () => (intro: x => x),
  union_case: (name, elems) => (intro: generate-value-intro(spec)),
  struct: (name, fields) => (
    intro: generate-constr(none, (__tag__: "constr-spec/fields", fields: fields))
  ),
  enum: (name, constrs) => {
    let intros = generate-enum-intros(constrs)
    (intro: intros, intros: intros)
  },
  array_case: (name, inner) => (intro: generate-value-intro(spec)),
  dictionary_case: (name, key, inner) => (intro: generate-value-intro(spec)),
  function_case: (name, dom, cod) => (intro: generate-function-intro(dom, cod)),
  fix: (name, fun) => generate-intro(fun(spec)),
  self: (.. args) => (:),
)(spec)

#let constr-spec-args(constr-spec) = constr-spec-elim(
  none_: arguments(),
  fields: fields => arguments(.. fields),
)(constr-spec)

#let CASES(spec, T) = spec-struct(
  .. spec-elim(
    empty_case: () => panic("empty specs do not have cases"),
    builtin: type_ => panic("builtin specs do not have cases"),
    any: () => panic("any specs do not have cases"),
    union_case: (name, elems) => panic("union specs do not have cases"),
    enum: (name, constrs) => constrs.pairs().map(((constr-name, constr-spec)) => {
      (constr-name, spec-function(.. constr-spec-args(constr-spec))(T))
    }).to-dict(),
    struct: (name, fields) => (mk: spec-function(.. fields)(T)),
    array_case: (name, inner) => panic("array specs do not have cases"),
    dictionary_case: (name, key, value) => panic("dictionary specs do not have cases"),
    function_case: (name, dom, cod) => panic("function specs do not have cases"),
    fix: (name, fun) => CASES(fun(spec), T),
    self: depth => panic("self specs do not have cases"),
  )(spec)
)

#let generate-elim(spec) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (elim: generate-value-elim(spec)),
  struct: (name, fields) => (elim: mk => value => {
    let fields = result-unwrap(validate(spec, value))
    if type(mk) == function {
      mk(.. fields.values())
    } else {
      mk
    }
  }),
  any: () => (elim: f => x => f(x)),
  union_case: (name, elems) => (elim: generate-value-elim(spec)),
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
  array_case: (name, inner) => (elim: generate-value-elim(spec)),
  dictionary_case: (name, key, inner) => (elim: generate-value-elim(spec)),
  function_case: (name, dom, cod) => (elim: generate-function-elim(dom, cod)),
  fix: (name, fun) => generate-elim(fun(spec)),
  self: (.. args) => (:),
)(spec)

#let generate-enum-field(spec, constrs, field-name) = value => {
  value = result-unwrap(validate(spec, value))
  let tag = value.remove("__tag__").split("/").last()
  let constr-spec = constrs.at(tag)
  let fields = result-unwrap(validate-constr(constr-spec, .. value))
  if fields.keys().contains(field-name) {
    fields.at(field-name)
  } else {
    panic("constructor `" + tag + "` does not have field `" + field-name + "`")
  }
}

#let generate-enum-fields(spec, constrs) = {
  let field-names = ()
  for constr-spec in constrs.values() {
    if constr-spec.__tag__ == "constr-spec/fields" {
      field-names += constr-spec.fields.keys()
    }
  }
  (
    fields: field-names.dedup().map(field-name => (
      field-name,
      generate-enum-field(spec, constrs, field-name),
    )).to-dict()
  )
}

#let generate-fields(spec) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (
    fields: fields.keys().map(field-name => (
      field-name,
      value => result-unwrap(validate(spec, value)).at(field-name),
    )).to-dict()
  ),
  enum: (name, constrs) => generate-enum-fields(spec, constrs),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
  fix: (name, fun) => generate-fields(fun(spec)),
  self: (.. args) => (:),
)(spec)

#let generate-rec(spec) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => (:),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
  fix: (name, fun) => spec-elim(
    empty_case: () => (:),
    builtin: type_ => (:),
    any: () => (:),
    union_case: (name, elems) => (:),
    struct: (name, fields) => (:),
    enum: (name, constrs) => generate-enum-rec(spec, constrs),
    array_case: (name, inner) => (:),
    dictionary_case: (name, key, inner) => (:),
    function_case: (name, dom, cod) => (:),
    fix: (name, fun) => (:),
    self: (.. args) => (:),
  )(fun(spec)),
  self: (.. args) => (:),
)(spec)

#let generate-visit(spec) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => (:),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
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
