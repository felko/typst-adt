#import "../../src/lib.typ" as adt: result-is-err

#let TOKEN = adt.enum(
  __name__: "TOKEN",
  eof: none,
  lit: int,
  span: (start: int, end: int),
)

#let (
  intro: (
    eof: token-eof,
    lit: token-lit,
    span: token-span,
  ),
  elim: token-elim,
) = adt.generate(TOKEN)

#assert.eq(token-eof.__tag__, "eof")
#assert.eq(token-lit(3).value, 3)
#assert.eq((adt.intro(TOKEN).lit)(4).value, 4)
#assert.eq(token-span(1, 4).start, 1)
#assert.eq(token-span(start: 2, end: 5).end, 5)

#let token-kind = token-elim(
  eof: "eof",
  lit: value => "lit:" + str(value),
  span: (start, end) => "span:" + str(end - start),
)

#assert.eq(token-kind(token-eof), "eof")
#assert.eq(token-kind(token-lit(9)), "lit:9")
#assert.eq(token-kind(token-span(3, 8)), "span:5")
#assert.eq(adt.elim(TOKEN, eof: "eof", lit: value => value, span: "span")(
  token-lit(10),
), 10)
#assert(result-is-err(adt.validate(TOKEN, (__tag__: "missing"))))
#assert(result-is-err(adt.validate-constr(TOKEN.constrs.span, 1)))
#assert(result-is-err(adt.validate-constr(TOKEN.constrs.span, 1, 2, 3)))
