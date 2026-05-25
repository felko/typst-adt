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
  intro: (
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
  intro: (
    nil: list-nil,
    cons: list-cons,
  ),
  fields: (
    head: list-head-field,
    tail: list-tail-field,
  ),
  elim: list-elim,
  rec: list-rec,
) = generate(LIST(int))

#let one-two = list-cons(1, list-cons(2, list-nil))
#assert.eq(list-nil, (__tag__: "nil"))
#assert.eq(list-cons(head: 1, tail: list-nil), (__tag__: "cons", head: 1, tail: list-nil))
#assert.eq(validate(LIST(int), one-two), ok(one-two))
#assert.eq(list-head-field(one-two), 1)
#assert.eq(list-tail-field(one-two), list-cons(2, list-nil))

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

#let list-append(l1, l2) = list-rec(
  nil: l2,
  cons: list-cons,
)(l1)

#let list(.. args) = {
  let xs = list-nil
  for elem in args.pos().rev() {
    xs = list-cons(elem, xs)
  }
  xs
}

#assert.eq(list-append(list(1, 2), list(3)), list(1, 2, 3))
