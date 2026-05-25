#import "../../src/lib.typ": *

#let NULLARY = spec-function()(int)
#let (intro: const-int, elim: apply-const-int) = generate(NULLARY)
#assert.eq(const-int(() => 7)(), 7)
#assert.eq(apply-const-int(const-int(() => 8))(), 8)
#assert(result-is-err(validate-args(NULLARY.dom, 1)))

#let UNARY = spec-function(int)(str)
#let (intro: int-to-str, elim: apply-int-to-str) = generate(UNARY)
#assert.eq(int-to-str(x => str(x))(4), "4")
#assert.eq(apply-int-to-str(int-to-str(x => str(x + 1)))(4), "5")
#assert(result-is-err(validate-args(UNARY.dom)))
#assert(result-is-err(validate-args(UNARY.dom, 1, 2)))

#let BINARY = spec-function(int, int)(int)
#let (intro: add, elim: apply-add) = generate(BINARY)
#assert.eq(add((x, y) => x + y)(2, 3), 5)
#assert.eq(apply-add(add((x, y) => x * y))(4, 5), 20)

#let NAMED = spec-function(left: int, right: int)(int)
#let (intro: named-add, elim: apply-named-add) = generate(NAMED)
#assert.eq(named-add((left: 0, right: 0) => left + right)(left: 2, right: 3), 5)
#assert.eq(
  apply-named-add(named-add((left: 0, right: 0) => left * right))(
    left: 4,
    right: 5,
  ),
  20,
)
#assert(result-is-err(validate-args(NAMED.dom, left: 1)))
#assert(result-is-err(validate-args(NAMED.dom, left: 1, right: "bad")))

