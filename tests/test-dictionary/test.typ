#import "../../src/lib.typ": *

#let STR-INTS = spec-dictionary(str, int)
#let (intro: str-ints, elim: str-ints-elim) = generate(STR-INTS)

#assert.eq((str-ints((:)).validate)(), ok(str-ints((:))))
#assert.eq(str-ints((a: 1, b: 2)).a, 1)
#assert.eq(str-ints-elim(xs => xs.len())(str-ints((a: 1, b: 2))), 2)
#assert.eq(str-ints-elim(xs => xs.at("a") + xs.at("b"))(str-ints((a: 1, b: 2))), 3)
#assert(result-is-err(validate(STR-INTS, (a: "bad"))))
