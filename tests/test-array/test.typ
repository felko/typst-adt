#import "../../src/lib.typ": *

#let (intro: ints, elim: ints-elim) = generate(spec-array(int))
#assert.eq(ints(()), ())
#assert.eq(ints((1, 2, 3)), (1, 2, 3))
#assert.eq(ints-elim(xs => xs.len())(ints((1, 2, 3))), 3)
#assert.eq(ints-elim(xs => xs.fold(0, (acc, x) => acc + x))(ints((1, 2, 3))), 6)
#assert(result-is-err(validate(spec-array(int), (1, "bad"))))

#let nested = spec-array(spec-array(int))
#let (intro: nested-intro, elim: nested-elim) = generate(nested)
#assert.eq(
  nested-elim(rows => rows.first().len())(nested-intro(((1, 2), (3, 4)))),
  2,
)

