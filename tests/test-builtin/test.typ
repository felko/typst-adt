#import "../../src/lib.typ": *

#let (intro: int-intro, elim: int-elim) = generate(int)
#assert.eq(int-intro(4), 4)
#assert.eq(int-elim(x => x + 1)(4), 5)
#assert.eq(int-elim("constant")(4), "constant")
#assert(result-is-err(validate(int, "4")))

#let (intro: any-intro, elim: any-elim) = generate(spec-any)
#assert.eq(any-intro((a: 1)).a, 1)
#assert.eq(any-elim(x => x.a)(any-intro((a: 7))), 7)

#assert.eq(generate(spec-empty).len(), 0)
#assert(result-is-err(validate(spec-empty, none)))
