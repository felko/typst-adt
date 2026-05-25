#import "../src/lib.typ": *

#set document(
  title: "typst-meta manual",
  author: "felko",
)
#set page(margin: 1in)
#set text(size: 10pt)
#show heading: it => {
  set block(above: 1.2em, below: 0.5em)
  it
}

= typst-meta

`typst-meta` is a Typst package for describing algebraic-style data types.
It provides specs, validation, generated constructors, eliminators, field
accessors, and recursive folds.

The package is experimental. The main entrypoint is:

```typst
#import "src/lib.typ": *
```

When writing from inside `docs/`, use:

```typst
#import "../src/lib.typ": *
```

== Core idea

A _spec_ describes the shape of a value.

- Builtin Typst types like `int`, `str`, and `bool` can be used directly.
- `validate(spec, value)` checks values and returns `ok(value)` or `err(msg)`.
- `generate(spec)` builds helpers for constructing and consuming values.

== Specs

=== Builtins

Use Typst builtin types directly:

```typst
#assert.eq(validate(int, 4), ok(4))
#assert(result-is-err(validate(int, "4")))
```

Use `spec-any` when any value is valid, and `spec-empty` when no value is valid.

=== Structs

Use `spec-struct` for dictionaries with fixed fields.

```typst
#let BOX = spec-struct(__name__: "BOX", value: int)
#let (intro: box, elim: box-elim) = generate(BOX)

#assert.eq(box(4).value, 4)
#assert.eq(box(value: 5).value, 5)
#assert.eq(box-elim(value => value + 1)(box(4)), 5)
```

=== Enums

Use `spec-enum` for tagged variants.

```typst
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

#let token-kind = token-elim(
  eof: "eof",
  lit: value => "lit:" + str(value),
  span: (start, end) => "span:" + str(end - start),
)

#assert.eq(token-kind(token-eof), "eof")
#assert.eq(token-kind(token-lit(9)), "lit:9")
#assert.eq(token-kind(token-span(3, 8)), "span:5")
```

Constructor shapes:

- `none`: no fields.
- `int`: one field named `value`.
- `(start: int, end: int)`: named fields.

=== Unions

Use `spec-union` when a value may match any of several specs.

```typst
#let INT-OR-STR = spec-union(int, str)
#let (intro: int-or-str, elim: int-or-str-elim) = generate(INT-OR-STR)

#assert.eq(int-or-str(4), 4)
#assert.eq(int-or-str("ok"), "ok")
#assert.eq(int-or-str-elim(value => str(value))(4), "4")
#assert(result-is-err(validate(INT-OR-STR, 1pt)))
```

Nested unions are flattened. `spec-union()` creates `spec-empty`.

=== Arrays

Use `spec-array(inner)` for arrays.

```typst
#let INTS = spec-array(int)
#let (intro: ints, elim: ints-elim) = generate(INTS)

#assert.eq(ints((1, 2, 3)), (1, 2, 3))
#assert.eq(ints-elim(xs => xs.len())(ints((1, 2, 3))), 3)
#assert(result-is-err(validate(INTS, (1, "bad"))))
```

=== Dictionaries

Use `spec-dictionary(key, value)` for dictionaries.

```typst
#let STR-INTS = spec-dictionary(str, int)
#let (intro: str-ints, elim: str-ints-elim) = generate(STR-INTS)

#assert.eq(str-ints((a: 1, b: 2)).a, 1)
#assert.eq(str-ints-elim(xs => xs.len())(str-ints((a: 1, b: 2))), 2)
#assert(result-is-err(validate(STR-INTS, (a: "bad"))))
```

=== Functions

Use `spec-function(..domain)(codomain)` for functions.

```typst
#let ADD = spec-function(int, int)(int)
#let (intro: add, elim: apply-add) = generate(ADD)

#assert.eq(add((x, y) => x + y)(2, 3), 5)
#assert.eq(apply-add(add((x, y) => x * y))(4, 5), 20)
```

Named arguments are supported:

```typst
#let NAMED = spec-function(left: int, right: int)(int)
#let (intro: named-add) = generate(NAMED)

#assert.eq(named-add((left: 0, right: 0) => left + right)(
  left: 2,
  right: 3,
), 5)
```

== Recursive specs

Use `spec-fix` for recursive data.

```typst
#let LIST(T) = {
  T = result-unwrap(spec-parse(T))
  spec-fix(
    __name__: "list(" + spec-to-string(T) + ")",
    self => spec-enum(
      nil: none,
      cons: (head: T, tail: self),
    ),
  )
}

#let (
  intro: (
    nil: list-nil,
    cons: list-cons,
  ),
  fields: (
    head: list-head,
    tail: list-tail,
  ),
  elim: list-elim,
  rec: list-rec,
) = generate(LIST(int))

#let list(..args) = {
  let xs = list-nil
  for elem in args.pos().rev() {
    xs = list-cons(elem, xs)
  }
  xs
}

#let one-two = list(1, 2)

#assert.eq(list-head(one-two), 1)
#assert.eq(list-tail(one-two).head, 2)

#let length = list-rec(
  nil: 0,
  cons: (head, tail-len) => tail-len + 1,
)

#assert.eq(length(one-two), 2)
```

Generated recursive enum values also carry methods:

```typst
#assert.eq((one-two.elim)(
  nil: none,
  cons: (head, tail) => head,
), 1)

#assert.eq((one-two.rec)(
  nil: 0,
  cons: (head, tail-len) => tail-len + 1,
), 2)
```

== Annotations

Recursive enum values can be annotated in one pass.

```typst
#let sized = (list(1, 2).annotate)(
  __ann__: (size: int),
  nil: (size: 0),
  cons: (head, tail) => (size: tail.size + 1),
)

#assert.eq(sized.size, 2)
#assert.eq(sized.tail.size, 1)
#assert.eq((sized.validate)(), ok(sized))
```

== Results

Results are plain dictionaries tagged as `result/ok` or `result/err`.

```typst
#let good = ok(4)
#let bad = err("expected int")

#assert.eq(good.value, 4)
#assert(result-is-ok(good))
#assert(result-is-err(bad))
```

Useful helpers:

- `result-map(f, result)`
- `result-map2(f, a, b)`
- `result-and-then(result, cont)`
- `result-all(f, xs)`
- `result-all-dict(f, xs)`
- `result-any(f, xs)`
- `result-unwrap(result)`

== Public functions

=== Spec builders

#table(
  columns: (1fr, 2fr),
  inset: 6pt,
  [`spec-builtin(type)`], [Builtin Typst type spec.],
  [`spec-any`], [Accepts any value.],
  [`spec-empty`], [Accepts no values.],
  [`spec-struct(__name__: auto, ..fields)`], [Struct spec.],
  [`spec-enum(__name__: auto, ..constructors)`], [Enum spec.],
  [`spec-union(__name__: auto, ..specs)`], [Union spec.],
  [`spec-array(__name__: auto, inner)`], [Array spec.],
  [`spec-dictionary(__name__: auto, key, value)`], [Dictionary spec.],
  [`spec-function(..domain)(codomain)`], [Function spec.],
  [`spec-fix(__name__: auto, fun)`], [Recursive spec.],
  [`spec-parse(spec)`], [Parses shorthand into an explicit spec.],
  [`spec-to-string(spec)`], [Renders a compact spec name.],
)

=== Validation and generation

#table(
  columns: (1fr, 2fr),
  inset: 6pt,
  [`validate(spec, value)`], [Checks a value against a spec.],
  [`validate-args(args-spec, ..args)`], [Checks function call arguments.],
  [`validate-constr(constr-spec, ..args)`], [Checks constructor arguments.],
  [`generate(spec)`], [Generates helpers for a spec.],
  [`CASES(spec, T)`], [Builds a spec for eliminator cases.],
)

=== Result helpers

#table(
  columns: (1fr, 2fr),
  inset: 6pt,
  [`RESULT(T)`], [Spec for result values.],
  [`ok(x)`], [Successful result.],
  [`err(e)`], [Failed result.],
  [`result-elim(ok: ..., err: ...)(result)`], [Pattern matches a result.],
  [`result-map(f, result)`], [Maps an ok value.],
  [`result-map2(f, a, b)`], [Maps two ok values.],
  [`result-or-else(a, b)`], [Fallback result.],
  [`result-and-then(result, cont)`], [Chains result operations.],
  [`result-is-ok(result)`], [Checks for ok.],
  [`result-is-err(result)`], [Checks for err.],
  [`result-all(f, xs)`], [Collects ok results over an array.],
  [`result-all-dict(f, xs)`], [Collects ok results over a dictionary.],
  [`result-any(f, xs)`], [Returns the first ok result.],
  [`result-unwrap(result)`], [Returns ok value or panics.],
)

== Development

Tests use Tytanic.

```sh
tt run
```

Run selected tests by name:

```sh
tt run test-enum test-struct
```

Compile this manual:

```sh
typst compile --root . docs/manual.typ /tmp/typst-meta-manual.pdf
```
