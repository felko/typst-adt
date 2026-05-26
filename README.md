# typst-meta

`typst-meta` is a Typst package for describing and working with algebraic-style
data types. It lets you define type specifications, validate values against
those specifications, and generate constructors, eliminators, field accessors,
and recursive folds for structured Typst data.

Featured types:

- builtin Typst types such as `int`, `str`, `array`, ...
- structs, enums, unions, typed arrays, dictionaries and functions
- recursive types through fixed points
- generated introduction and elimination helpers
- runtime validation with `ok` / `err` result values
- annotations on recursive enum values

## Installation

For local development, import the package entrypoint directly:

```typst
#import "src/lib.typ": *
```

From another Typst project, copy or vendor this package and import
`src/lib.typ`, or install it as a local Typst package once you have chosen a
package namespace.

## Quick Start

Define a specification, generate helpers from it, and use the generated
constructors and eliminator:

```typst
#import "src/lib.typ": *

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

## Specifications

Specifications describe the shape of values. Most builders accept builtin Typst
types directly, so `int` is equivalent to a builtin integer spec.

```typst
#let INT-OR-STR = spec-union(int, str)
#let INT-ARRAY = spec-array(int)
#let STR-INTS = spec-dictionary(str, int)

#let BOX = spec-struct(
  __name__: "BOX",
  value: int,
)

#let ADD = spec-function(int, int)(int)
```

Enums use constructor names as named arguments. A constructor can take no value
with `none`, a single positional value spec, or named fields:

```typst
#let SHAPE = spec-enum(
  __name__: "SHAPE",
  point: none,
  circle: (radius: int),
  rect: (width: int, height: int),
)
```

## Validation

Use `validate(spec, value)` to check a value. Validation returns `ok(value)` or
`err(message)`, so callers can handle failures without immediately panicking.

```typst
#assert.eq(validate(int, 4), ok(4))
#assert(result-is-err(validate(int, "4")))

#let BOX = spec-struct(value: int)
#assert.eq(validate(BOX, (value: 4)), ok((value: 4)))
#assert(result-is-err(validate(BOX, (value: "bad"))))
```

Use `result-unwrap(result)` when you want to panic on an error and continue with
the validated value.

## Generated Helpers

`generate(spec)` returns a dictionary of helpers depending on the spec kind.
Common fields include:

- `intro`: a constructor or group of constructors
- `elim`: an eliminator or pattern-match-style dispatcher
- `fields`: generated field accessors for structs and enums
- `rec`: a recursive fold for recursive enum specs
- `annotate`: a recursive annotation pass for recursive enum specs

For structs:

```typst
#let PAIR = spec-struct(__name__: "PAIR", left: int, right: int)
#let (intro: pair, elim: pair-elim) = generate(PAIR)

#assert.eq(pair(2, 3).left, 2)
#assert.eq(pair(left: 4, right: 5).right, 5)
#assert.eq(pair-elim((left, right) => left * 10 + right)(pair(6, 7)), 67)
```

For functions:

```typst
#let ADD = spec-function(int, int)(int)
#let (intro: add, elim: apply-add) = generate(ADD)

#assert.eq(add((x, y) => x + y)(2, 3), 5)
#assert.eq(apply-add(add((x, y) => x * y))(4, 5), 20)
```

## Recursive Types

Use `spec-fix` to define recursive specifications. Generated values are plain
data; use the helpers returned by `generate` to eliminate, fold, annotate, or
validate them.

```typst
#let LIST(T) = spec-fix(
  __name__: "list(" + spec-to-string(T) + ")",
  self => spec-enum(
    nil: none,
    cons: (head: T, tail: self),
  ),
)

#let (
  intro: (
    nil: list-nil,
    cons: list-cons,
  ),
  rec: list-rec,
) = generate(LIST(int))

#let list(..args) = {
  let xs = list-nil
  for elem in args.pos().rev() {
    xs = list-cons(elem, xs)
  }
  xs
}

#let list-len = list-rec(
  nil: 0,
  cons: (head, tail-len) => tail-len + 1,
)

#assert.eq(list-len(list(1, 2, 3)), 3)
```

## Result Helpers

The package includes a small result type:

```typst
#let value = ok(4)
#assert.eq(value.value, 4)

#let failure = err("expected int")
#assert(result-is-err(failure))
```

Useful helpers include `result-map`, `result-map2`, `result-all`,
`result-all-dict`, `result-any`, `result-and-then`, and `result-unwrap`.

## Development

The test suite is organized via [`tytanic`](https://github.com/typst-community/tytanic), execute `tt run` to run the test suite.

## Repository Layout

- `src/lib.typ`: package entrypoint
- `src/spec.typ`: public spec constructors and meta-specs
- `src/validate.typ`: validation logic
- `src/generate.typ`: helper generation
- `src/result.typ`: result type and combinators
- `src/bootstrap.typ`: parsing, eliminators, and string rendering for specs
- `tests/`: assertion-based Typst examples
