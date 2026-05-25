#import "../../src/lib.typ": *

#assert.eq(result-unwrap(spec-parse(RESULT(int))).__tag__, "spec/enum")
#assert.eq(validate(SPEC, SPEC), ok(SPEC))
#assert.eq(validate(SPEC, spec-array(int)), ok(spec-array(int)))
#assert.eq(validate(SPEC, result-unwrap(spec-parse(RESULT(int)))), ok(
  result-unwrap(spec-parse(RESULT(int))),
))
#assert.eq(validate(SPEC, spec-array(RESULT(int))), ok(spec-array(RESULT(int))))
#assert.eq(validate(SPEC, spec-struct(x: int)), ok(spec-struct(x: int)))
#assert.eq(validate(SPEC, spec-function(int)(str)), ok(spec-function(int)(str)))
#assert.eq(validate(SPEC, spec-dictionary(str, spec-array(int))), ok(
  spec-dictionary(str, spec-array(int)),
))

#assert.eq(constr-spec-parse(RESULT(int).constrs.ok).value.fields.keys(), (
  "value",
))
#assert.eq(ok(4).value, 4)
#assert.eq(validate(RESULT(int), ok(4)), ok(ok(4)))
#assert(result-is-err(validate(RESULT(int), (bad: 4))))
