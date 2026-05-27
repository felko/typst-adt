#import "../../src/lib.typ" as adt: ok, result-is-err

#let (intro: ints, elim: ints-elim) = adt.generate(adt.array(int))
#assert.eq(ints(()), ())
#assert.eq(ints((1, 2, 3)), (1, 2, 3))
#assert.eq(ints-elim(xs => xs.len())(ints((1, 2, 3))), 3)
#assert.eq(ints-elim(xs => xs.fold(0, (acc, x) => acc + x))(ints((1, 2, 3))), 6)
#assert(result-is-err(adt.validate(adt.array(int), (1, "bad"))))

#let tree = ((), ((), (), ((), ((),)), ()))
#assert.eq(adt.validate(adt.fix(adt.array), tree), ok(tree))

#let nested = adt.array(adt.array(int))
#let (intro: nested-intro, elim: nested-elim) = adt.generate(nested)
#assert.eq(
  nested-elim(rows => rows.first().len())(nested-intro(((1, 2), (3, 4)))),
  2,
)
