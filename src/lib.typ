#import "result.typ" as result: (
  err, ok, result-all, result-all-dict, result-and-then, result-any,
  result-is-err, result-is-ok, result-map, result-map2, result-or-else,
  result-trace, result-validate, result-zip, result-zip-dict,
)
#import "spec.typ": (
  SPEC, TRACE, any, args-spec-fields, args-spec-null, args-spec-parse, array,
  builtin, constr-spec-parse, dict, dictionary, empty, enum, fix, fun,
  spec-parse, struct, union,
)
#import "validate.typ": validate, validate-args, validate-constr
#import "generate.typ": annotate, elim, generate, intro, rec, repr
#import "bootstrap.typ": result-error-to-string, to-string, trace-to-string

#let result-unwrap = result.result-unwrap-with.with(result-error-to-string)

#let RESULT(T) = result.RESULT-AUX(
  to-string,
  TRACE,
  result.result-unwrap(spec-parse(T)),
)
