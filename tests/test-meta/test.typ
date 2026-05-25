#import "../../src/lib.typ": *

#assert.eq(result-unwrap(spec-parse(RESULT(int))).__tag__, "spec/enum")
#assert.eq(result-unwrap(validate(SPEC, SPEC)).__tag__, "spec/fix")
#assert.eq(result-unwrap(validate(SPEC, spec-array(RESULT(int)))).__tag__, "spec/fix")

#assert.eq(constr-spec-parse(RESULT(int).constrs.ok).value.fields.keys(), ("value",))
#assert.eq(ok(4).value, 4)
#assert.eq(result-unwrap(validate(RESULT(int), ok(4))).value, 4)
#assert(result-is-err(validate(RESULT(int), (bad: 4))))

