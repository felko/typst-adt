#import "../../src/lib.typ" as adt: result-is-err

#let NULLARY = adt.fun()(int)
#let (intro: const-int, elim: apply-const-int) = adt.generate(NULLARY)
#assert.eq(const-int(() => 7)(), 7)
#assert.eq(apply-const-int(const-int(() => 8))(), 8)
#assert(result-is-err(adt.validate-args(NULLARY.dom, 1)))

#let UNARY = adt.fun(int)(str)
#let (intro: int-to-str, elim: apply-int-to-str) = adt.generate(UNARY)
#assert.eq(int-to-str(x => str(x))(4), "4")
#assert.eq(apply-int-to-str(int-to-str(x => str(x + 1)))(4), "5")
#assert(result-is-err(adt.validate-args(UNARY.dom)))
#assert(result-is-err(adt.validate-args(UNARY.dom, 1, 2)))

#let BINARY = adt.fun(int, int)(int)
#let (intro: add, elim: apply-add) = adt.generate(BINARY)
#assert.eq(add((x, y) => x + y)(2, 3), 5)
#assert.eq(apply-add(add((x, y) => x * y))(4, 5), 20)

#let NAMED = adt.fun(left: int, right: int)(int)
#let (intro: named-add, elim: apply-named-add) = adt.generate(NAMED)
#assert.eq(named-add((left: 0, right: 0) => left + right)(left: 2, right: 3), 5)
#assert.eq(
  apply-named-add(named-add((left: 0, right: 0) => left * right))(
    left: 4,
    right: 5,
  ),
  20,
)
#assert(result-is-err(adt.validate-args(NAMED.dom, left: 1)))
#assert(result-is-err(adt.validate-args(NAMED.dom, left: 1, right: "bad")))

