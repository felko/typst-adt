#import "../../src/lib.typ": *

#let UNIT = adt.struct(__name__: "UNIT")
#let (intro: unit, elim: unit-elim) = generate(UNIT)
#assert.eq(validate(UNIT, unit()), ok(unit()))
#assert.eq(unit-elim(() => "unit")(unit()), "unit")
#assert.eq(unit-elim("constant")(unit()), "constant")

#let BOX = adt.struct(__name__: "BOX", value: int)
#let (intro: box, elim: box-elim) = generate(BOX)
#assert.eq(box(4).value, 4)
#assert.eq(box(value: 5).value, 5)
#assert.eq(box-elim(value => value + 1)(box(4)), 5)
#assert(result-is-err(validate(BOX, (value: "bad"))))

#let PAIR = adt.struct(__name__: "PAIR", left: int, right: int)
#let (intro: pair, elim: pair-elim) = generate(PAIR)
#assert.eq(pair(2, 3).left, 2)
#assert.eq(pair(left: 4, right: 5).right, 5)
#assert.eq(pair-elim((left, right) => left * 10 + right)(pair(6, 7)), 67)
#assert(result-is-err(validate-constr(
  (__tag__: "constr-spec/fields", fields: PAIR.fields),
  1,
)))
#assert(result-is-err(validate-constr(
  (__tag__: "constr-spec/fields", fields: PAIR.fields),
  1,
  2,
  3,
)))
