#import "../../src/lib.typ": *

#let TOKEN = spec-enum(
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
) = generate(TOKEN)

#assert.eq(token-eof, (__tag__: "eof"))
#assert.eq(token-lit(3), (__tag__: "lit", value: 3))
#assert.eq(token-span(1, 4), (__tag__: "span", start: 1, end: 4))
#assert.eq(token-span(start: 2, end: 5), (__tag__: "span", start: 2, end: 5))

#let token-kind = token-elim(
  eof: "eof",
  lit: value => "lit:" + str(value),
  span: (start, end) => "span:" + str(end - start),
)

#assert.eq(token-kind(token-eof), "eof")
#assert.eq(token-kind(token-lit(9)), "lit:9")
#assert.eq(token-kind(token-span(3, 8)), "span:5")
#assert(result-is-err(validate(TOKEN, (__tag__: "missing"))))
#assert(result-is-err(validate-constr(TOKEN.constrs.span, 1)))
#assert(result-is-err(validate-constr(TOKEN.constrs.span, 1, 2, 3)))
