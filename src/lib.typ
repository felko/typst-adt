#import "result.typ" as result: ok, err, result-validate, result-is-ok, result-is-err, result-trace, result-any, result-and-then, result-or-else, result-map, result-map2, result-all, result-all-dict, result-zip, result-zip-dict
#import "spec.typ": SPEC, TRACE, builtin, enum, struct, fix, union, fun, any, empty, array, dict, dictionary, spec-parse, args-spec-null, args-spec-fields, args-spec-parse, constr-spec-parse
#import "validate.typ": validate, validate-constr, validate-args
#import "generate.typ": generate, annotate, rec
#import "bootstrap.typ": to-string, trace-to-string, result-error-to-string

#let result-unwrap = result.result-unwrap-with.with(result-error-to-string)

#let RESULT(T) = result.RESULT-AUX(
  to-string,
  TRACE,
  result.result-unwrap(spec-parse(T)),
)
