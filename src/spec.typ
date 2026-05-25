#import "bootstrap.typ": *

// spec-builtin(type)
#let spec-builtin(type_) = {
  assert(
    type(type_) == type,
    message: "expected type, got " + repr(type(type_)),
  )
  (
    __tag__: "spec/builtin",
    name: str(type_),
    value: type_,
  )
}

#let args-spec-none = (__tag__: "args-spec/none")
#let args-spec-fields-aux(spec-parse, pos, named) = (
  __tag__: "args-spec/args",
  pos: result-unwrap(result-all(validate, pos)),
  named: result-unwrap(result-all-dict(validate, named)),
)

#let constr-spec-none = (__tag__: "constr-spec/none")
#let constr-spec-fields-aux(spec-parse, fields) = (
  __tag__: "constr-spec/fields",
  fields: result-unwrap(result-all-dict(validate, fields)),
)

#let spec-any = (
  __tag__: "spec/any",
)

#let spec-empty = (
  __tag__: "spec/empty",
)

#let spec-enum(__name__: auto, ..args) = {
  assert(
    args.pos().len() == 0,
    message: "expected no positional argument",
  )
  let constrs = (:)
  let errs = (:)
  for (constr-name, constr-spec) in args.named().pairs() {
    let constr-spec = constr-spec-parse(constr-spec)
    if constr-spec.__tag__ == "result/ok" {
      constrs.insert(constr-name, constr-spec.value)
    } else if constr-spec.__tag__ == "result/err" {
      errs.insert(constr-name, constr-spec.msg)
    } else {
      panic("invalid result: `" + repr(constr-spec) + "`")
    }
  }
  if errs.len() == 1 {
    let (constr-name, constr-err-msg) = errs.pairs().first()
    panic(
      "invalid constructor specification for enum type in `"
        + constr-name
        + "`: "
        + constr-err-msg,
    )
  } else if errs.len() > 1 {
    panic(
      "invalid constructor specifications for enum type:\n"
        + errs
          .pairs()
          .map(((constr-name, constr-err-msg)) => (
            "  · In `" + constr-name + "`: " + constr-err-msg
          ))
          .join("\n"),
    )
  }
  (
    __tag__: "spec/enum",
    name: __name__,
    constrs: constrs,
  )
}

#let spec-struct(__name__: auto, ..args) = {
  assert(
    args.pos().len() == 0,
    message: "unexpected arguments: " + repr(arguments(..args.pos())),
  )
  let fields = (:)
  let errs = (:)
  for (field-name, field-spec) in args.named().pairs() {
    field-spec = spec-parse(field-spec)
    if field-spec.__tag__ == "result/ok" {
      fields.insert(field-name, field-spec.value)
    } else if field-spec.__tag__ == "result/err" {
      errs.insert(field-name, field-spec.msg)
    } else {
      panic("invalid result: `" + repr(field-spec) + "`")
    }
  }
  if errs.len() == 1 {
    let (constr-name, constr-err-msg) = errs.pairs().first()
    panic(
      "invalid field specification for enum type in `"
        + constr-name
        + "`: "
        + constr-err-msg,
    )
  } else if errs.len() > 1 {
    panic(
      "invalid field specifications for enum type:\n"
        + errs
          .pairs()
          .map(((constr-name, constr-err-msg)) => (
            "  · In `" + constr-name + "`: " + constr-err-msg
          ))
          .join("\n"),
    )
  }
  (
    __tag__: "spec/struct",
    name: __name__,
    fields: fields,
  )
}

#let spec-functor(__name__: auto, fun) = (
  __tag__: "spec/functor",
  name: __name__,
  apply: fun,
)

#let spec-fix(__name__: auto, fun) = {
  if type(fun) != function {
    panic("not a functor: `" + repr(fun) + "`")
  }
  (
    __tag__: "spec/fix",
    name: __name__,
    fun: fun,
  )
}

#let spec-union-elems(spec) = spec-elim(
  empty_case: () => (),
  builtin: type_ => (spec,),
  any: () => (spec,),
  union_case: (name, elems) => elems,
  enum: (name, constrs) => (spec,),
  struct: (name, fields) => (spec,),
  array_case: (name, inner) => (spec,),
  dictionary_case: (name, key, value) => (spec,),
  function_case: (name, dom, cod) => (spec,),
  fix: (name, fun) => (spec,),
  self: depth => (spec,),
)(spec)

#let spec-union2(left, right) = {
  let elems = (spec-union-elems(left) + spec-union-elems(right)).dedup()
  if elems.len() == 0 {
    spec-empty
  } else if elems.len() == 1 {
    elems.first()
  } else {
    (
      __tag__: "spec/union",
      name: auto,
      elems: elems,
    )
  }
}

#let spec-union(__name__: auto, ..args) = {
  assert(
    args.named().len() == 0,
    message: "unexpected arguments: " + repr(arguments(..args.named())),
  )
  let unioned = args
    .pos()
    .map(spec => result-unwrap(spec-parse(spec)))
    .fold(spec-empty, spec-union2)
  unioned.name = __name__
  unioned
}

#let spec-array(__name__: auto, inner) = {
  result-unwrap(result-map(
    inner => (
      __tag__: "spec/array",
      name: __name__,
      inner: inner,
    ),
    spec-parse(inner),
  ))
}

#let spec-dictionary(__name__: auto, key, value) = {
  result-unwrap(result-map2(
    (key, value) => (
      __tag__: "spec/dictionary",
      name: __name__,
      key: key,
      value: value,
      // validate: result-all.with(inner.validate)
    ),
    spec-parse(key),
    spec-parse(value),
  ))
}

#let spec-function(..dom) = cod => {
  result-unwrap(result-map2(
    (dom, cod) => (
      __tag__: "spec/function",
      name: auto,
      dom: dom,
      cod: cod,
    ),
    args-spec-parse(dom),
    spec-parse(cod),
  ))
}

#let SPEC-NAME = spec-union(str, type(auto))

#let ARGS-SPEC(T) = spec-enum(
  __name__: "args-spec(" + spec-to-string(T) + ")",
  ..(
    ("none", none),
    (
      "args",
      (
        pos: spec-array(T),
        named: spec-dictionary(str, T),
      ),
    ),
  ).to-dict(),
)

#let CONSTR-SPEC(T) = spec-enum(
  __name__: "constr-spec(" + spec-to-string(T) + ")",
  ..(
    ("none", none),
    (
      "fields",
      (
        fields: spec-dictionary(str, T),
      ),
    ),
  ).to-dict(),
)

#let SPEC = spec-fix(
  __name__: "spec",
  self => spec-enum(
    __name__: "spec-shape",
    empty: none,
    builtin: (name: SPEC-NAME, value: type),
    any: none,
    enum: (name: SPEC-NAME, constrs: spec-dictionary(str, CONSTR-SPEC(self))),
    struct: (name: SPEC-NAME, fields: spec-dictionary(str, self)),
    union: (name: SPEC-NAME, elems: spec-array(self)),
    array: (name: SPEC-NAME, inner: self),
    dictionary: (name: SPEC-NAME, key: self, value: self),
    function: (name: SPEC-NAME, dom: ARGS-SPEC(self), cod: self),
    fix: (name: str, fun: spec-function(self)(self)),
    self: (depth: int),
  ),
)
