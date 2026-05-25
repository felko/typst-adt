#import "bootstrap.typ": *
#import "validate.typ": *

#let validate-runtime-constr(
  validate-runtime,
  constr-spec,
  value,
) = constr-spec-elim(
  none_: ok(value),
  fields: field-specs => {
    for (field-name, field-spec) in field-specs.pairs() {
      if not value.keys().contains(field-name) {
        return err("missing field `" + field-name + "`")
      }
      let result = validate-runtime(field-spec, value.at(field-name))
      if result-is-err(result) {
        return result
      }
    }
    ok(value)
  },
)(constr-spec)

#let validate-runtime(spec, value) = spec-elim(
  empty_case: () => err("empty type has no values"),
  builtin: type_ => result-map(_ => value, validate(spec, value)),
  any: () => ok(value),
  union_case: (name, elems) => result-map(
    _ => value,
    result-any(elem => validate-runtime(elem, value), elems),
  ),
  enum: (name, constrs) => {
    if type(value) == dictionary and value.keys().contains("__tag__") {
      let tag = value.__tag__.split("/").last()
      if constrs.keys().contains(tag) {
        validate-runtime-constr(validate-runtime, constrs.at(tag), value)
      } else {
        err("unknown constructor `" + value.__tag__ + "`")
      }
    } else {
      err("not an enum value: `" + repr(value) + "`")
    }
  },
  struct: (name, fields) => validate-runtime-constr(
    validate-runtime,
    (__tag__: "constr-spec/fields", fields: fields),
    value,
  ),
  array_case: (name, inner) => result-map(_ => value, validate(spec, value)),
  dictionary_case: (name, key, inner) => result-map(
    _ => value,
    validate(spec, value),
  ),
  function_case: (name, dom, cod) => result-map(
    _ => value,
    validate(spec, value),
  ),
  fix: (name, fun) => validate-runtime(fun(spec), value),
  self: depth => err("cannot validate unresolved self spec"),
)(spec)

#let attach-ops(spec, value, ops: (:)) = {
  if type(value) != dictionary {
    return value
  }
  let value = value
  if ops.keys().contains("elim") {
    value.insert("elim", (..cases) => (ops.elim)(..cases)(value))
  }
  if ops.keys().contains("rec") {
    value.insert("rec", (..cases) => (ops.rec)(..cases)(value))
  }
  if ops.keys().contains("annotate") {
    value.insert("annotate", (..cases) => (ops.annotate)(..cases)(value))
  }
  let validate-method() = {
    let result = validate-runtime(spec, value)
    if result-is-err(result) {
      result
    } else {
      ok((..result.value, validate: validate-method))
    }
  }
  value.insert("validate", validate-method)
  value
}

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

#let project-constr-args(constr-spec, ..args) = constr-spec-elim(
  none_: {
    let named = strip-value-method-fields(args.named())
    let annotations = named.remove("__ann__", default: (:))
    assert(
      type(annotations) == dictionary,
      message: "expected `__ann__` to be a dictionary",
    )
    if args.pos().len() != 0 or named.len() != 0 {
      err("expected no arguments, got `" + repr(args) + "`")
    } else {
      ok(annotations)
    }
  },
  fields: field-specs => {
    let (pos, named) = (args.pos(), strip-value-method-fields(args.named()))
    let annotations = named.remove("__ann__", default: (:))
    assert(
      type(annotations) == dictionary,
      message: "expected `__ann__` to be a dictionary",
    )
    let fields = (:)
    for (field-name, field-spec) in field-specs.pairs() {
      let arg = if named.keys().contains(field-name) {
        named.remove(field-name)
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
    if pos.len() > 0 or named.len() > 0 {
      err("unrecognizd arguments: `" + repr(arguments(..pos, ..named)) + "`")
    } else {
      for (ann-name, ann-value) in annotations.pairs() {
        fields.insert(ann-name, ann-value)
      }
      ok(fields)
    }
  },
)(constr-spec)

#let generate-constr(tag, constr-spec) = constr-spec-elim(
  none_: if tag == none { attach-ops(auto, (:)) } else {
    attach-ops(auto, (__tag__: tag))
  },
  fields: _ => {
    if tag == none {
      (..args) => attach-ops(auto, result-unwrap(validate-constr(
        constr-spec,
        ..args,
      )))
    } else {
      (..args) => attach-ops(auto, (
        __tag__: tag,
        ..result-unwrap(validate-constr(constr-spec, ..args)),
      ))
    }
  },
)(constr-spec)

#let generate-constr-with-spec(
  spec,
  tag,
  constr-spec,
  ops: (:),
) = constr-spec-elim(
  none_: if tag == none { attach-ops(spec, (:), ops: ops) } else {
    attach-ops(spec, (__tag__: tag), ops: ops)
  },
  fields: _ => {
    if tag == none {
      (..args) => attach-ops(
        spec,
        result-unwrap(project-constr-args(constr-spec, ..args)),
        ops: ops,
      )
    } else {
      (..args) => attach-ops(
        spec,
        (
          __tag__: tag,
          ..result-unwrap(project-constr-args(constr-spec, ..args)),
        ),
        ops: ops,
      )
    }
  },
)(constr-spec)

#let generate-value-intro(spec, ops: (:)) = value => attach-ops(
  spec,
  result-unwrap(validate(spec, value)),
  ops: ops,
)

#let generate-value-elim(spec) = f => value => {
  let value = result-unwrap(validate(spec, value))
  if type(f) == function {
    f(value)
  } else {
    f
  }
}

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

#let generate-function-elim(dom, cod) = value => (..args) => {
  let value = result-unwrap(validate(
    (__tag__: "spec/function", name: auto, dom: dom, cod: cod),
    value,
  ))
  let args = result-unwrap(validate-args(dom, ..args))
  result-unwrap(validate(cod, value(..args)))
}

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

#let generate-rec-field(go, field-spec, value) = {
  if spec-is-fix(field-spec) {
    go(value)
  } else {
    value
  }
}

#let generated-method-field-names = ("validate", "elim", "rec", "annotate")

#let extra-value-fields(constr-spec, value) = {
  let extras = value
  let _ = extras.remove("__tag__", default: none)
  for field-name in generated-method-field-names {
    let _ = extras.remove(field-name, default: none)
  }
  if constr-spec.__tag__ == "constr-spec/fields" {
    for field-name in constr-spec.fields.keys() {
      let _ = extras.remove(field-name, default: none)
    }
  }
  extras
}

#let merge-extra-fields(value, extras) = {
  if type(value) != dictionary {
    return value
  }
  for (field-name, field-value) in extras.pairs() {
    if not value.keys().contains(field-name) {
      value.insert(field-name, field-value)
    }
  }
  value
}

#let rebuild-with-extra-fields(spec, tag, fields, extras, value) = {
  if extras.len() == 0 {
    return value
  }
  let rebuilt = (__tag__: tag, ..fields)
  for (field-name, field-value) in extras.pairs() {
    rebuilt.insert(field-name, field-value)
  }
  if not rebuilt.keys().contains("depth") {
    rebuilt.insert("depth", value)
  }
  attach-ops(spec, rebuilt)
}

#let generate-enum-rec(spec, constrs) = (
  rec: (..cases) => {
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
    let go(value) = {
      if type(value) != dictionary or not value.keys().contains("__tag__") {
        panic("not an enum value: `" + repr(value) + "`")
      }
      let tag = value.remove("__tag__").split("/").last()
      if not constrs.keys().contains(tag) {
        panic("unknown constructor `" + tag + "`")
      }
      let constr-spec = constrs.at(tag)
      let fields = result-unwrap(project-constr(constr-spec, value))
      let extras = extra-value-fields(constr-spec, value)
      let finish(result) = {
        if type(result) == dictionary {
          merge-extra-fields(result, extras)
        } else {
          rebuild-with-extra-fields(spec, tag, fields, extras, result)
        }
      }
      let case = cases.at(tag)
      if type(case) != function {
        return finish(case)
      }
      let result = if constr-spec.__tag__ == "constr-spec/none" {
        case()
      } else if constr-spec.__tag__ == "constr-spec/fields" {
        let mapped = (:)
        for (field-name, field-spec) in constr-spec.fields.pairs() {
          mapped.insert(field-name, generate-rec-field(
            go,
            field-spec,
            fields.at(field-name),
          ))
        }
        case(..mapped.values())
      } else {
        panic("ill-formed constructor spec: `" + repr(constr-spec) + "`")
      }
      if type(result) == function {
        (..args) => finish(result(..args))
      } else {
        finish(result)
      }
    }
    go
  },
)

#let generate-enum-intros(spec, constrs, ops: (:)) = (
  constrs
    .pairs()
    .map(((constr-name, constr-spec)) => {
      (
        constr-name,
        generate-constr-with-spec(spec, constr-name, constr-spec, ops: ops),
      )
    })
    .to-dict()
)

#let generate-intro(spec, ops: (:)) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (intro: generate-value-intro(spec, ops: ops)),
  any: () => (intro: value => attach-ops(spec, value, ops: ops)),
  union_case: (name, elems) => (intro: generate-value-intro(spec, ops: ops)),
  struct: (name, fields) => (
    intro: generate-constr-with-spec(
      spec,
      none,
      (__tag__: "constr-spec/fields", fields: fields),
      ops: ops,
    ),
  ),
  enum: (name, constrs) => {
    let intros = generate-enum-intros(spec, constrs, ops: ops)
    (intro: intros, intros: intros)
  },
  array_case: (name, inner) => (intro: generate-value-intro(spec, ops: ops)),
  dictionary_case: (name, key, inner) => (
    intro: generate-value-intro(spec, ops: ops),
  ),
  function_case: (name, dom, cod) => (intro: generate-function-intro(dom, cod)),
  fix: (name, fun) => spec-elim(
    empty_case: () => (:),
    builtin: type_ => (:),
    any: () => (:),
    union_case: (name, elems) => (:),
    struct: (name, fields) => (
      intro: generate-constr-with-spec(
        spec,
        none,
        (__tag__: "constr-spec/fields", fields: fields),
        ops: ops,
      ),
    ),
    enum: (name, constrs) => {
      let intros = generate-enum-intros(spec, constrs, ops: ops)
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

#let constr-spec-args(constr-spec) = constr-spec-elim(
  none_: arguments(),
  fields: fields => arguments(..fields),
)(constr-spec)

#let CASES(spec, T) = spec-struct(
  ..spec-elim(
    empty_case: () => panic("empty specs do not have cases"),
    builtin: type_ => panic("builtin specs do not have cases"),
    any: () => panic("any specs do not have cases"),
    union_case: (name, elems) => panic("union specs do not have cases"),
    enum: (name, constrs) => constrs
      .pairs()
      .map(((constr-name, constr-spec)) => {
        (constr-name, spec-function(..constr-spec-args(constr-spec))(T))
      })
      .to-dict(),
    struct: (name, fields) => (mk: spec-function(..fields)(T)),
    array_case: (name, inner) => panic("array specs do not have cases"),
    dictionary_case: (name, key, value) => panic(
      "dictionary specs do not have cases",
    ),
    function_case: (name, dom, cod) => panic(
      "function specs do not have cases",
    ),
    fix: (name, fun) => CASES(fun(spec), T),
    self: depth => panic("self specs do not have cases"),
  )(spec),
)

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
  array_case: (name, inner) => (elim: generate-value-elim(spec)),
  dictionary_case: (name, key, inner) => (elim: generate-value-elim(spec)),
  function_case: (name, dom, cod) => (elim: generate-function-elim(dom, cod)),
  fix: (name, fun) => generate-elim(fun(spec)),
  self: (..args) => (:),
)(spec)

#let generate-enum-field(spec, constrs, field-name) = value => {
  if type(value) != dictionary or not value.keys().contains("__tag__") {
    panic("not an enum value: `" + repr(value) + "`")
  }
  let tag = value.remove("__tag__").split("/").last()
  if not constrs.keys().contains(tag) {
    panic("unknown constructor `" + tag + "`")
  }
  let constr-spec = constrs.at(tag)
  let fields = result-unwrap(project-constr(constr-spec, value))
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
    fields: field-names
      .dedup()
      .map(field-name => (
        field-name,
        generate-enum-field(spec, constrs, field-name),
      ))
      .to-dict(),
  )
}

#let generate-fields(spec) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (
    fields: fields
      .keys()
      .map(field-name => (
        field-name,
        value => result-unwrap(validate(spec, value)).at(field-name),
      ))
      .to-dict(),
  ),
  enum: (name, constrs) => generate-enum-fields(spec, constrs),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
  fix: (name, fun) => generate-fields(fun(spec)),
  self: (..args) => (:),
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
    self: (..args) => (:),
  )(fun(spec)),
  self: (..args) => (:),
)(spec)

#let generate-enum-annotate(spec, constrs, ops: (:)) = (
  annotate: (..args) => {
    assert(
      args.pos().len() == 0,
      message: "expected no positional arguments, got " + repr(args.pos()),
    )
    let named = args.named()
    assert(
      named.keys().contains("__ann__"),
      message: "missing `__ann__` annotation spec",
    )
    let ann-specs = named.remove("__ann__")
    assert(
      type(ann-specs) == dictionary,
      message: "expected `__ann__` to be a dictionary",
    )
    ann-specs = result-unwrap(result-all-dict(spec-parse, ann-specs))
    let cases = named
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
    let go(value) = {
      if type(value) != dictionary or not value.keys().contains("__tag__") {
        panic("not an enum value: `" + repr(value) + "`")
      }
      let tag = value.remove("__tag__").split("/").last()
      if not constrs.keys().contains(tag) {
        panic("unknown constructor `" + tag + "`")
      }
      let constr-spec = constrs.at(tag)
      let fields = result-unwrap(project-constr(constr-spec, value))
      let rebuilt = (__tag__: tag)
      let algebra-fields = (:)
      if constr-spec.__tag__ == "constr-spec/fields" {
        for (field-name, field-spec) in constr-spec.fields.pairs() {
          let field-value = fields.at(field-name)
          if spec-is-fix(field-spec) {
            let annotated-child = go(field-value)
            rebuilt.insert(field-name, annotated-child)
            algebra-fields.insert(
              field-name,
              ann-specs
                .keys()
                .map(ann-name => (ann-name, annotated-child.at(ann-name)))
                .to-dict(),
            )
          } else {
            rebuilt.insert(field-name, field-value)
            algebra-fields.insert(field-name, field-value)
          }
        }
      }
      let case = cases.at(tag)
      let ann-value = if type(case) == function {
        case(..algebra-fields.values())
      } else {
        case
      }
      ann-value = result-unwrap(validate(
        (__tag__: "spec/struct", name: auto, fields: ann-specs),
        ann-value,
      ))
      for (ann-name, ann-field-value) in ann-value.pairs() {
        rebuilt.insert(ann-name, ann-field-value)
      }
      attach-ops(spec, rebuilt, ops: ops)
    }
    go
  },
)

#let generate-annotate(spec, ops: (:)) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => generate-enum-annotate(spec, constrs, ops: ops),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
  fix: (name, fun) => spec-elim(
    empty_case: () => (:),
    builtin: type_ => (:),
    any: () => (:),
    union_case: (name, elems) => (:),
    struct: (name, fields) => (:),
    enum: (name, constrs) => generate-enum-annotate(spec, constrs, ops: ops),
    array_case: (name, inner) => (:),
    dictionary_case: (name, key, inner) => (:),
    function_case: (name, dom, cod) => (:),
    fix: (name, fun) => (:),
    self: (..args) => (:),
  )(fun(spec)),
  self: (..args) => (:),
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
  self: (..args) => (:),
)(spec)

#let generate(spec) = {
  let ops = (:)
  let elim = generate-elim(spec)
  let fields = generate-fields(spec)
  let rec = generate-rec(spec)
  let visit = generate-visit(spec)
  if elim.keys().contains("elim") {
    ops.insert("elim", elim.elim)
  }
  if rec.keys().contains("rec") {
    ops.insert("rec", rec.rec)
  }
  let annotate = generate-annotate(spec, ops: ops)
  if annotate.keys().contains("annotate") {
    ops.insert("annotate", annotate.annotate)
  }
  (:
    ..generate-intro(spec, ops: ops),
    ..elim,
    ..fields,
    ..rec,
    ..annotate,
    ..visit,
  )
}
