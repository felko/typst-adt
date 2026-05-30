#import "../../src/lib.typ" as adt: ok
#import "../../src/methods/common.typ": project-constr, project-constr-args, result-trace-constr

#let array-error = adt.validate(adt.array(int), (1, "bad"))
#assert.eq(array-error.msg, "expected a value of type `integer`, got `string`")
#assert.eq(array-error.trace.__tag__, "trace/val")
#assert.eq(array-error.trace.cont.__tag__, "trace/array-val")
#assert.eq(array-error.trace.cont.index, 1)
#assert.eq(array-error.trace.cont.cont.__tag__, "trace/val")
#assert.eq(array-error.trace.cont.cont.cont.__tag__, "trace/root")
#assert.eq(adt.validate(adt.TRACE, array-error.trace), ok(array-error.trace))
#assert.eq(
  adt.result-error-to-string(array-error),
  "error:
  • expected a value of type `integer`, got `string`
  • when checking the value:
      \"bad\"
      against the expected type:
        integer
  • in array element at index 1:
      \"bad\"
  • when checking the value:
      (1, \"bad\")
      against the expected type:
        array(integer)",
)

#let dict-error = adt.validate(adt.dict(int), (good: 1, bad: "bad"))
#assert.eq(dict-error.trace.cont.__tag__, "trace/dictionary-val")
#assert.eq(dict-error.trace.cont.key, "bad")
#assert.eq(adt.validate(adt.TRACE, dict-error.trace), ok(dict-error.trace))

#let args-error = adt.validate-args(adt.fun(int)(str).dom, "bad")
#assert.eq(args-error.trace.__tag__, "trace/args-pos-arg")
#assert.eq(args-error.trace.index, 0)
#assert.eq(adt.validate(adt.TRACE, args-error.trace), ok(args-error.trace))

#let args-arity-error = adt.validate-args(adt.fun(int)(str).dom)
#assert.eq(args-arity-error.trace.__tag__, "trace/args-arity")
#assert.eq(adt.validate(adt.TRACE, args-arity-error.trace), ok(args-arity-error.trace))
#assert(adt.result-error-to-string(args-arity-error).contains("expected call shape"))

#let named-dom = adt.fun(left: int)(str).dom
#let args-extra-error = adt.validate-args(named-dom, left: 1, right: 2)
#assert.eq(args-extra-error.trace.__tag__, "trace/args-extra-named")
#assert.eq(adt.validate(adt.TRACE, args-extra-error.trace), ok(args-extra-error.trace))
#assert(adt.result-error-to-string(args-extra-error).contains("unexpected named arguments"))

#let args-missing-error = adt.validate-args(named-dom)
#assert.eq(args-missing-error.trace.__tag__, "trace/args-missing-named")
#assert.eq(adt.validate(adt.TRACE, args-missing-error.trace), ok(args-missing-error.trace))
#assert(adt.result-error-to-string(args-missing-error).contains("missing named arguments"))

#let constr-spec = adt.constr-spec-parse((value: int)).value
#let constr-missing-error = adt.validate-constr(constr-spec)
#assert.eq(constr-missing-error.trace.__tag__, "trace/constr-missing-arg")
#assert.eq(adt.validate(adt.TRACE, constr-missing-error.trace), ok(constr-missing-error.trace))
#assert(adt.result-error-to-string(constr-missing-error).contains("missing argument"))

#let constr-extra-error = adt.validate-constr(constr-spec, 1, 2)
#assert.eq(constr-extra-error.trace.__tag__, "trace/constr-extra-args")
#assert.eq(adt.validate(adt.TRACE, constr-extra-error.trace), ok(constr-extra-error.trace))
#assert(adt.result-error-to-string(constr-extra-error).contains("extra arguments"))

#let projected-missing-error = project-constr(constr-spec, (:))
#assert.eq(projected-missing-error.trace.__tag__, "trace/constr-missing-field")
#assert.eq(adt.validate(adt.TRACE, projected-missing-error.trace), ok(projected-missing-error.trace))
#assert(adt.result-error-to-string(projected-missing-error).contains("missing field"))

#let projected-extra-error = project-constr-args(constr-spec, 1, 2)
#assert.eq(projected-extra-error.trace.__tag__, "trace/constr-extra-args")
#assert.eq(adt.validate(adt.TRACE, projected-extra-error.trace), ok(projected-extra-error.trace))

#let named-constr-error = result-trace-constr(
  adt.fix(__name__: "tree(string)", self => self),
  "leaf",
  adt.validate(int, "bad"),
)
#assert.eq(named-constr-error.trace.__tag__, "trace/constr")
#assert.eq(named-constr-error.trace.name, "tree-leaf")
#assert.eq(adt.validate(adt.TRACE, named-constr-error.trace), ok(named-constr-error.trace))
#assert(adt.result-error-to-string(named-constr-error).contains("constructor `tree-leaf`"))

#let struct-error = adt.validate(adt.struct(value: int), "bad")
#assert.eq(struct-error.trace.__tag__, "trace/val")
#assert.eq(adt.validate(adt.TRACE, struct-error.trace), ok(struct-error.trace))

#let leaf-error = adt.validate(int, "bad")
#assert.eq(leaf-error.trace.__tag__, "trace/val")
#assert.eq(leaf-error.trace.cont.__tag__, "trace/root")

#let empty-error = adt.validate(adt.empty, none)
#assert.eq(empty-error.trace.__tag__, "trace/val")
#assert.eq(empty-error.trace.cont.__tag__, "trace/root")

#let union-error = adt.validate(adt.union(int, str), none)
#assert.eq(union-error.trace.cont.__tag__, "trace/union")
#assert.eq(adt.validate(adt.TRACE, union-error.trace), ok(union-error.trace))
