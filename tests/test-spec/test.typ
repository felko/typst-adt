#import "../../src/lib.typ": *

#validate(SPEC, SPEC)

#constr-spec-parse(RESULT(int).constrs.ok)

#ok(4)

#validate-constr(RESULT(int).constrs.ok, .. arguments(4))

#validate(RESULT(int), ok(4))

#let VEC = spec-struct(
  __name__: "VEC",
  x: length,
  y: length,
)

#spec-to-string(VEC)

#result-unwrap(validate-constr((__tag__: "constr-spec/fields", fields: VEC.fields), .. (x: 10pt, y: 1cm)))

#result-unwrap(validate(VEC, (x: 10pt, y: 3em)))

#let OPTION(T) = {
  T = result-unwrap(spec-parse(T))
  spec-enum(
    __name__: "OPTION(" + spec-to-string(T) + ")",
    nothing: none,
    some: T,
  )
}

#let (
  intros: (
    nothing: option-nothing,
    some: option-some,
  ),
  elim: option-elim,
) = generate(OPTION(spec-any))

#let option-unwrap = option-elim(
  nothing: () => panic("attempted to unwrap nothing"),
  some: value => value
)

#let LIST(T) = {
  T = result-unwrap(spec-parse(T))
  spec-fix(
    __name__: "list(" + spec-to-string(T) + ")",
    self => spec-enum(
      __name__: "list.base(" + spec-to-string(T) + ", " + spec-to-string(self) + ")",
      nil: none,
      cons: (head: T, tail: self),
    ),
  )
}

#let (
  intros: (
    nil: list-nil,
    cons: list-cons,
  ),
  elim: list-elim,
  rec: list-rec,
) = generate(LIST(int))

#(LIST(int).fun)(LIST(int))

#result-unwrap(validate(LIST(int), list-nil))

#result-unwrap(validate(LIST(int), list-cons(1, list-cons(2, list-nil))))

#result-unwrap(validate(LIST(int), list-cons(head: 1, tail: list-cons(head: 2, tail: list-nil))))

#let list-head = list-elim(
  nil: option-nothing,
  cons: (head, _) => option-some(head)
)

#option-unwrap(list-head(list-cons(1, list-nil)))