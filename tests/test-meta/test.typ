#import "../../src/lib.typ" as adt: (
  RESULT, SPEC, ok, result-and-then, result-unwrap,
)

#assert.eq(result-unwrap(adt.spec-parse(RESULT(int))).__tag__, "spec/enum")
#assert.eq(adt.validate(SPEC, SPEC), ok(SPEC))
#assert.eq(adt.validate(SPEC, adt.array(int)), ok(adt.array(int)))
#assert.eq(adt.validate(SPEC, RESULT(int)), ok(RESULT(int)))
#assert.eq(adt.validate(SPEC, adt.array(RESULT(int))), ok(adt.array(
  RESULT(int),
)))
#assert.eq(adt.validate(SPEC, adt.struct(x: int)), ok(adt.struct(x: int)))
#assert.eq(
  result-unwrap(adt.spec-parse(adt.struct(x: adt.array(int)))),
  adt.struct(x: adt.array(int)),
)
#assert.eq(adt.validate(SPEC, adt.fun(int)(str)), ok(adt.fun(int)(str)))
#assert.eq(adt.validate(SPEC, adt.dict(adt.array(int))), ok(
  adt.dictionary(adt.array(int)),
))
#assert.eq(adt.to-string(adt.fix(self => self)), "fix self@0. self@0")

#assert.eq(adt.constr-spec-parse(RESULT(int).constrs.ok).value.fields.keys(), (
  "value",
))
#assert.eq(ok(4).value, 4)
#assert.eq(adt.validate(RESULT(int), ok(4)), ok(ok(4)))
#assert(adt.result-is-err(adt.validate(RESULT(int), (bad: 4))))
