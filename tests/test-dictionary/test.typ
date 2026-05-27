#import "../../src/lib.typ" as adt: ok, result-is-err

#let STR-INTS = adt.dict(str, int)
#let (intro: str-ints, elim: str-ints-elim) = adt.generate(STR-INTS)

#assert.eq(adt.validate(STR-INTS, str-ints((:))), ok(str-ints((:))))
#assert.eq(str-ints((a: 1, b: 2)).a, 1)
#assert.eq(str-ints-elim(xs => xs.len())(str-ints((a: 1, b: 2))), 2)
#assert.eq(
  str-ints-elim(xs => xs.at("a") + xs.at("b"))(str-ints((a: 1, b: 2))),
  3,
)
#assert(result-is-err(adt.validate(STR-INTS, (a: "bad"))))
