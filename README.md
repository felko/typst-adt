# typst-adt

`typst-adt` is a Typst package for describing and working with algebraic data
types. It provides specifications, runtime validation, generated constructors,
eliminators, field accessors, recursive folds, and annotation passes for plain
Typst values.

The Typst package is named `adt`, so published imports can stay short:

```typst
#import "@preview/adt:0.1.0" as adt: ok, result-is-err
```

For local development, import the entrypoint directly:

```typst
#import "src/lib.typ" as adt: ok, result-is-err
```

## Features

- Builtin Typst types such as `int`, `str`, `array`, and `dictionary`.
- Structs, enums, unions, typed arrays, dictionaries, and functions.
- Recursive types through fixed-point specs.
- Generated constructors, eliminators, field accessors, and recursive folds.
- Runtime validation with `ok` / `err` result values.
- Annotation passes for recursive enum values.

## Quick Start

Define a spec, generate helpers from it, and use the generated constructors and
eliminator:

```typst
#import "src/lib.typ" as adt

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
) = adt.generate(TOKEN)

#let token-kind = token-elim(
  eof: "eof",
  lit: value => "lit:" + str(value),
  span: (start, end) => "span:" + str(end - start),
)

#assert.eq(token-kind(token-eof), "eof")
#assert.eq(token-kind(token-lit(9)), "lit:9")
#assert.eq(token-kind(token-span(3, 8)), "span:5")
```

## Specs

Specs describe the shape of values. Most builders accept builtin Typst types
directly, so `int` is equivalent to `adt.builtin(int)`.

```typst
#let INT-OR-STR = adt.union(int, str)
#let INT-ARRAY = adt.array(int)
#let STR-INTS = adt.dict(str, int)
#let ADD = adt.fun(int, int)(int)

#let BOX = adt.struct(
  __name__: "BOX",
  value: int,
)
```

Enums use constructor names as named arguments. A constructor can be `none`, a
single value spec, or named fields:

```typst
#let SHAPE = adt.enum(
  __name__: "SHAPE",
  point: none,
  circle: (radius: int),
  rect: (width: int, height: int),
)
```

## Validation

Use `adt.validate(spec, value)` to check a value. Validation returns
`ok(value)` or `err(message)`, so callers can handle failures without
immediately panicking.

```typst
#import "src/lib.typ" as adt: ok, result-is-err

#assert.eq(adt.validate(int, 4), ok(4))
#assert(result-is-err(adt.validate(int, "4")))

#let BOX = adt.struct(value: int)
#assert.eq(adt.validate(BOX, (value: 4)), ok((value: 4)))
#assert(result-is-err(adt.validate(BOX, (value: "bad"))))
```

Use `adt.result-unwrap(result)` when you want to panic on an error and continue
with the validated value.

## Generated Helpers

`adt.generate(spec)` returns a dictionary of helpers depending on the spec kind.
Common fields include:

- `intro`: a constructor or group of constructors.
- `elim`: an eliminator or pattern-match-style dispatcher.
- `fields`: generated field accessors for structs and enums.
- `rec`: a recursive fold for recursive enum specs.
- `annotate`: a recursive annotation pass for recursive enum specs.

For structs:

```typst
#let PAIR = adt.struct(__name__: "PAIR", left: int, right: int)
#let (intro: pair, elim: pair-elim) = adt.generate(PAIR)

#assert.eq(pair(2, 3).left, 2)
#assert.eq(pair(left: 4, right: 5).right, 5)
#assert.eq(pair-elim((left, right) => left * 10 + right)(pair(6, 7)), 67)
```

For functions:

```typst
#let ADD = adt.fun(int, int)(int)
#let (intro: add, elim: apply-add) = adt.generate(ADD)

#assert.eq(add((x, y) => x + y)(2, 3), 5)
#assert.eq(apply-add(add((x, y) => x * y))(4, 5), 20)
```

## Recursive Types

Use `adt.fix` to define recursive specs. Generated values are plain data; use
the helpers returned by `adt.generate` to eliminate, fold, annotate, or validate
them.

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
  rec: list-rec,
) = adt.generate(LIST(int))

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

Use `adt.rec(spec, ..cases)` when you want a recursive fold without first
destructuring `adt.generate(spec)`.

## Results

The package includes a small result type:

```typst
#let value = adt.ok(4)
#assert.eq(value.value, 4)

#let failure = adt.err("expected int")
#assert(adt.result-is-err(failure))
```

Useful helpers include `adt.result-map`, `adt.result-map2`, `adt.result-all`,
`adt.result-all-dict`, `adt.result-any`, `adt.result-and-then`, and
`adt.result-unwrap`.

## Development

The test suite uses [`tytanic`](https://github.com/typst-community/tytanic):

```sh
tt run
```

## Repository Layout

- `src/lib.typ`: package entrypoint.
- `src/spec.typ`: public spec constructors and meta specs.
- `src/validate.typ`: validation logic.
- `src/generate.typ`: helper generation facade.
- `src/methods/`: generated helper families.
- `src/result.typ`: result type and combinators.
- `src/bootstrap.typ`: parsing, eliminators, and string rendering for specs.
- `tests/`: assertion-based Typst examples.
