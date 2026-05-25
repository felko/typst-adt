#import "../../src/lib.typ": *

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
) = generate(OPTION(int))

#assert.eq(option-nothing, (__tag__: "nothing"))
#assert.eq(option-some(4), (__tag__: "some", value: 4))
#assert.eq(option-elim(nothing: 0, some: value => value)(option-nothing), 0)
#assert.eq(option-elim(nothing: 0, some: value => value)(option-some(5)), 5)

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

#let one-two = list-cons(1, list-cons(2, list-nil))
#assert.eq(list-nil, (__tag__: "nil"))
#assert.eq(list-cons(head: 1, tail: list-nil), (__tag__: "cons", head: 1, tail: list-nil))
#assert.eq(result-unwrap(validate(LIST(int), one-two)), one-two)

#let list-head = list-elim(
  nil: none,
  cons: (head, tail) => head,
)
#assert.eq(list-head(one-two), 1)

#let list-len = list-rec(
  nil: 0,
  cons: (head, tail-len) => tail-len + 1,
)
#assert.eq(list-len(list-nil), 0)
#assert.eq(list-len(one-two), 2)
