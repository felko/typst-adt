#import "../../src/lib.typ" as adt: result-is-err

#let INT-OR-STR = adt.union(int, str)
#let (intro: int-or-str, elim: int-or-str-elim) = adt.generate(INT-OR-STR)

#assert.eq(int-or-str(4), 4)
#assert.eq(int-or-str("ok"), "ok")
#assert.eq(int-or-str-elim(value => str(value))(int-or-str(4)), "4")
#assert.eq(int-or-str-elim(value => value)(int-or-str("ok")), "ok")
#assert(result-is-err(adt.validate(INT-OR-STR, 1pt)))

#let NESTED = adt.union(adt.union(int, str), bool)
#let (intro: nested-intro, elim: nested-elim) = adt.generate(NESTED)
#assert.eq(nested-elim(value => value)(nested-intro(true)), true)
#assert(result-is-err(adt.validate(NESTED, none)))

#assert.eq(adt.union().__tag__, "spec/empty")

