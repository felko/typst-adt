#import "../src/lib.typ": *
#import "@preview/tidy:0.4.3"

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

Use `adt.any` when any value is valid, and `adt.empty` when no value is valid.

=== Structs

Use `adt.struct` for dictionaries with fixed fields.

```typst
#let BOX = adt.struct(__name__: "BOX", value: int)
#let (intro: box, elim: box-elim) = generate(BOX)

#assert.eq(box(4).value, 4)
#assert.eq(box(value: 5).value, 5)
#assert.eq(box-elim(value => value + 1)(box(4)), 5)
```

=== Enums

Use `adt.enum` for tagged variants.

```typst
#let TOKEN = adt.enum(
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

Use `adt.union` when a value may match any of several specs.

```typst
#let INT-OR-STR = adt.union(int, str)
#let (intro: int-or-str, elim: int-or-str-elim) = generate(INT-OR-STR)

#assert.eq(int-or-str(4), 4)
#assert.eq(int-or-str("ok"), "ok")
#assert.eq(int-or-str-elim(value => str(value))(4), "4")
#assert(result-is-err(validate(INT-OR-STR, 1pt)))
```

Nested unions are flattened. `adt.union()` creates `adt.empty`.

=== Arrays

Use `adt.array(inner)` for arrays.

```typst
#let INTS = adt.array(int)
#let (intro: ints, elim: ints-elim) = generate(INTS)

#assert.eq(ints((1, 2, 3)), (1, 2, 3))
#assert.eq(ints-elim(xs => xs.len())(ints((1, 2, 3))), 3)
#assert(result-is-err(validate(INTS, (1, "bad"))))
```

=== Dictionaries

Use `dict(key, value)` for dictionaries.

```typst
#let STR-INTS = dict(str, int)
#let (intro: str-ints, elim: str-ints-elim) = generate(STR-INTS)

#assert.eq(str-ints((a: 1, b: 2)).a, 1)
#assert.eq(str-ints-elim(xs => xs.len())(str-ints((a: 1, b: 2))), 2)
#assert(result-is-err(validate(STR-INTS, (a: "bad"))))
```

=== Functions

Use `fun(..domain)(codomain)` for functions.

```typst
#let ADD = fun(int, int)(int)
#let (intro: add, elim: apply-add) = generate(ADD)

#assert.eq(add((x, y) => x + y)(2, 3), 5)
#assert.eq(apply-add(add((x, y) => x * y))(4, 5), 20)
```

Named arguments are supported:

```typst
#let NAMED = fun(left: int, right: int)(int)
#let (intro: named-add) = generate(NAMED)

#assert.eq(named-add((left: 0, right: 0) => left + right)(
  left: 2,
  right: 3,
), 5)
```

== Recursive specs

Use `adt.fix` for recursive data.

```typst
#let LIST(T) = adt.fix(
  __name__: "list(" + adt.to-string(T) + ")",
  self => adt.enum(
    nil: none,
    cons: (head: T, tail: self),
  ),
)

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
  annotate: list-annotate,
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

Generated values are plain data. Use generated helpers explicitly:

```typst
#assert.eq(list-elim(
  nil: none,
  cons: (head, tail) => head,
)(one-two), 1)

#assert.eq(list-rec(
  nil: 0,
  cons: (head, tail-len) => tail-len + 1,
)(one-two), 2)
```

== Annotations

Recursive enum values can be annotated in one pass.

```typst
#let SIZED-LIST = adt.annotate(LIST(int), size: int)

#let sized = list-annotate(
  __ann__: (size: int),
  nil: (size: 0),
  cons: (head, tail) => (size: tail.size + 1),
)(list(1, 2))

#assert.eq(sized.size, 2)
#assert.eq(sized.tail.size, 1)
#assert.eq(validate(SIZED-LIST, sized), ok(sized))
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

== Reference

#let show-reference(title, path) = {
  heading(level: 2, title)
  tidy.show-module(tidy.parse-module(read(path)))
}

#show-reference("Specs", "../src/spec.typ")
#show-reference("Validation", "../src/validate.typ")
#show-reference("Generation", "../src/generate.typ")
#show-reference("Results", "../src/result.typ")
#show-reference("Parsing", "../src/bootstrap.typ")

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
