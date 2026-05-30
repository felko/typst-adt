# typst-adt

`adt` is a Typst package for describing algebraic data types and generic type wrappers over builtin types. From the description of a type, `adt` generates corresponding validation functions, constructors, eliminators, field accessors,
recursors, and annotation passes.

It has the following properties:
- First-class: specs are regular Typst values, functors are regular Typst functions from specs to specs;
- Bootstrapped: the specs themselves are described by an algebraic datatype and are able to be validated as well as matched on;
- Non-intrusive: besides sum types (`enum`s) which have a designated tag field, the values need not to carry any sort of type information;
- Non-infectious: as a consequence of the above, neither dependencies nor dependents need conversion.

## Features

- Handles Typst types such as `int`, `str`, `array`, and `dictionary` as well as finer grained generic variants e.g. `adt.array(T)`
- Type formers like structs and enums
- Type operators like `union`
- Recursive types through fixed-point specs.
- Generated constructors, eliminators, field accessors, and recursive folds.
- Recoverable validation errors (as opposed to panicking checks)
- Annotation passes for recursive enum values.

## Simple example

Define a spec, generate helpers from it, and use the generated constructors and
eliminator:

```typst
#let STYLE = adt.struct(
  size: length,
  color: color
)
#let (
  intro: style,
  elim: style-elim
) = adt.generate(STYLE)

#assert.eq(
  style(12pt, red),
  (size: 12pt, color: red)
)
#assert.eq(
  style-elim(
    (size, color) => style(
      size + 8pt,
      color
    )
  )(style(12pt, red)),
  style(20pt, red)
)
```

## Recursive Types

Use `adt.fix` to take the fixpoint of any Typst function as a mapping between specs. 

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

## Annotations

Annotate a full 

## Development

The test suite uses [`tytanic`](https://github.com/typst-community/tytanic):

```sh
tt run
```
