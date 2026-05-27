#import "../../src/lib.typ" as adt: SPEC, RESULT, ok, result-unwrap

#assert.eq(result-unwrap(adt.spec-parse(RESULT(int))).__tag__, "spec/enum")
#assert.eq(adt.validate(SPEC, SPEC), ok(SPEC))
#assert.eq(adt.validate(SPEC, adt.array(int)), ok(adt.array(int)))
#assert.eq(adt.validate(SPEC, result-unwrap(adt.spec-parse(RESULT(int)))), ok(
  result-unwrap(adt.spec-parse(RESULT(int))),
))
#assert.eq(adt.validate(SPEC, adt.array(RESULT(int))), ok(adt.array(RESULT(int))))
#assert.eq(adt.validate(SPEC, adt.struct(x: int)), ok(adt.struct(x: int)))
#assert.eq(adt.validate(SPEC, adt.fun(int)(str)), ok(adt.fun(int)(str)))
#assert.eq(adt.validate(SPEC, dict(str, adt.array(int))), ok(
  dict(str, adt.array(int)),
))

#assert.eq(constr-spec-parse(RESULT(int).constrs.ok).value.fields.keys(), (
  "value",
))
#assert.eq(ok(4).value, 4)
#assert.eq(validate(RESULT(int), ok(4)), ok(ok(4)))
#assert(result-is-err(validate(RESULT(int), (bad: 4))))
