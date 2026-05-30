#import "bootstrap.typ": *
#import "std.typ" as std

/// Builds a spec for a builtin Typst type.
///
/// - `type_`: A Typst type such as `int`, `str`, or `dictionary`.
/// -> spec
#let builtin(
  /// Builtin Typst type.
  /// -> type
  type_,
) = {
  assert(
    std.type(type_) == std.type,
    message: "expected type, got " + repr(std.type(type_)),
  )
  (
    __tag__: "spec/builtin",
    name: str(type_),
    value: type_,
  )
}

/// Argument spec for functions that take no arguments.
/// -> args-spec
#let args-spec-null = (__tag__: "args-spec/null")

/// Builds an argument spec from positional and named specs.
///
/// This is an internal constructor used after parsing shorthand argument specs.
/// -> args-spec
#let args-spec-fields-aux(
  /// Spec parser to apply recursively.
  /// -> function
  spec-parse,
  /// Positional argument specs.
  /// -> array(spec)
  pos,
  /// Named argument specs.
  /// -> dictionary(str, spec)
  named,
) = (
  __tag__: "args-spec/args",
  pos: result-unwrap(result-all(validate, pos)),
  named: result-unwrap(result-all-dict(validate, named)),
)

#let args-spec-fields = args-spec-fields-aux.with(spec-parse)

/// Constructor spec for constructors with no fields.
/// -> constr-spec
#let constr-spec-null = (__tag__: "constr-spec/null")

/// Builds a constructor spec from named field specs.
///
/// This is an internal constructor used after parsing shorthand constructor
/// specs.
/// -> constr-spec
#let constr-spec-fields-aux(
  /// Spec parser to apply recursively.
  /// -> function
  spec-parse,
  /// Constructor field specs.
  /// -> dictionary(str, spec)
  fields,
) = (
  __tag__: "constr-spec/fields",
  fields: result-unwrap(result-all-dict(validate, fields)),
)

/// Spec that accepts any value.
/// -> spec
#let any = (
  __tag__: "spec/any",
)

/// Spec that accepts no values.
/// -> spec
#let empty = (
  __tag__: "spec/union",
  name: "empty",
  elems: (),
)

/// Builds an enum spec from named constructors.
///
/// Constructors are passed as named arguments. Use `none` for a nullary
/// constructor, a spec for a single `value` field, or a dictionary/argument
/// value for named fields.
/// -> spec
#let enum(
  /// Optional display name.
  /// -> str | auto
  __name__: auto,
  /// Named constructor specs.
  /// -> arguments
  ..args,
) = {
  assert(
    args.pos().len() == 0,
    message: "expected no positional argument",
  )
  let constrs = (:)
  let errs = (:)
  for (constr-name, constr-spec) in args.named().pairs() {
    let constr-spec = constr-spec-parse(constr-spec)
    if constr-spec.__tag__ == "result/ok" {
      constrs.insert(constr-name, constr-spec.value)
    } else if constr-spec.__tag__ == "result/err" {
      errs.insert(constr-name, constr-spec.msg)
    } else {
      panic("invalid result: `" + repr(constr-spec) + "`")
    }
  }
  if errs.len() == 1 {
    let (constr-name, constr-err-msg) = errs.pairs().first()
    panic(
      "invalid constructor specification for enum type in `"
        + constr-name
        + "`: "
        + constr-err-msg,
    )
  } else {
    assert.eq(
      errs.len(),
      0,
      message: "invalid constructor specifications for enum type:\n"
        + errs
          .pairs()
          .map(((constr-name, constr-err-msg)) => (
            "  · In `" + constr-name + "`: " + constr-err-msg
          ))
          .join("\n"),
    )
  }
  (
    __tag__: "spec/enum",
    name: __name__,
    constrs: constrs,
  )
}

/// Builds a struct spec from named fields.
///
/// Fields are passed as named arguments and parsed as specs.
/// -> spec
#let struct(
  /// Optional display name.
  /// -> str | auto
  __name__: auto,
  /// Named field specs.
  /// -> arguments
  ..args,
) = {
  assert(
    args.pos().len() == 0,
    message: "unexpected arguments: " + repr(arguments(..args.pos())),
  )
  let fields = (:)
  let errs = (:)
  for (field-name, field-spec) in args.named().pairs() {
    field-spec = spec-parse(field-spec)
    if field-spec.__tag__ == "result/ok" {
      fields.insert(field-name, field-spec.value)
    } else if field-spec.__tag__ == "result/err" {
      errs.insert(field-name, field-spec.msg)
    } else {
      panic("invalid result: `" + repr(field-spec) + "`")
    }
  }
  if errs.len() == 1 {
    let (constr-name, constr-err-msg) = errs.pairs().first()
    panic(
      "invalid field specification for enum type in `"
        + constr-name
        + "`: "
        + constr-err-msg,
    )
  } else if errs.len() > 1 {
    panic(
      "invalid field specifications for enum type:\n"
        + errs
          .pairs()
          .map(((constr-name, constr-err-msg)) => (
            "  · In `" + constr-name + "`: " + constr-err-msg
          ))
          .join("\n"),
    )
  }
  (
    __tag__: "spec/struct",
    name: __name__,
    fields: fields,
  )
}

/// Builds a recursive spec as a fixed point.
///
/// - `fun`: Function from the recursive self spec to the unfolded base spec.
/// -> spec
#let fix(
  /// Optional display name.
  /// -> str | auto
  __name__: auto,
  /// Function from self spec to unfolded spec.
  /// -> function
  fun,
) = {
  if std.type(fun) != std.function {
    panic("not a functor: `" + repr(fun) + "`")
  }
  (
    __tag__: "spec/fix",
    name: __name__,
    fun: fun,
  )
}

/// Adds annotation fields to a spec.
///
/// For enum specs, every constructor receives the annotation fields. For
/// fixed-point specs, recursive children are annotated too.
/// -> spec
#let annotate(
  /// Spec to annotate.
  /// -> spec
  spec,
  /// Annotation field specs.
  /// -> arguments
  ..ann,
) = {
  assert(
    ann.pos().len() == 0,
    message: "expected no positional annotation specs",
  )
  let ann-fields = result-unwrap(result-all-dict(spec-parse, ann.named()))
  let add-fields(fields) = {
    for field-name in ann-fields.keys() {
      if fields.keys().contains(field-name) {
        panic("annotation field already exists: `" + field-name + "`")
      }
    }
    (:..fields, ..ann-fields)
  }
  let add-ann(spec) = spec-elim(
    builtin: type_ => panic("cannot annotate builtin spec"),
    any: () => panic("cannot annotate any spec"),
    union: (name, elems) => panic("cannot annotate union spec"),
    enum: (name, constrs) => (
      __tag__: "spec/enum",
      name: name,
      constrs: constrs
        .pairs()
        .map(((constr-name, constr-spec)) => (
          constr-name,
          constr-spec-elim(
            null: (__tag__: "constr-spec/fields", fields: ann-fields),
            fields: fields => (
              __tag__: "constr-spec/fields",
              fields: add-fields(fields),
            ),
          )(constr-spec),
        ))
        .to-dict(),
    ),
    struct: (name, fields) => (
      __tag__: "spec/struct",
      name: name,
      fields: add-fields(fields),
    ),
    array: (name, inner) => panic("cannot annotate array spec"),
    dictionary: (name, value) => panic(
      "cannot annotate dictionary spec",
    ),
    function: (name, dom, cod) => panic("cannot annotate function spec"),
    fix: (name, fun) => fix(
      __name__: name,
      self => add-ann(fun(self)),
    ),
    self: depth => spec,
  )(result-unwrap(spec-parse(spec)))
  add-ann(spec)
}

/// Returns the flattened elements of a union spec.
///
/// Non-union specs are returned as a one-element array.
/// -> array(spec)
#let union-elems(
  /// Spec to inspect.
  /// -> spec
  spec,
) = spec-elim(
  builtin: type_ => (spec,),
  any: () => (spec,),
  union: (name, elems) => elems,
  enum: (name, constrs) => (spec,),
  struct: (name, fields) => (spec,),
  array: (name, inner) => (spec,),
  dictionary: (name, value) => (spec,),
  function: (name, dom, cod) => (spec,),
  fix: (name, fun) => (spec,),
  self: depth => (spec,),
)(spec)

/// Combines two specs into one flattened union.
///
/// Empty unions collapse away and one-element unions collapse to that element.
/// -> spec
#let union2(
  /// Left spec.
  /// -> spec
  left,
  /// Right spec.
  /// -> spec
  right,
) = {
  let elems = (union-elems(left) + union-elems(right)).dedup()
  if elems.len() == 0 {
    empty
  } else if elems.len() == 1 {
    elems.first()
  } else {
    (
      __tag__: "spec/union",
      name: auto,
      elems: elems,
    )
  }
}

/// Builds a union spec from zero or more specs.
///
/// Nested unions are flattened. Calling with no specs returns `empty`.
/// -> spec
#let union(
  /// Optional display name.
  /// -> str | auto
  __name__: auto,
  /// Specs to union.
  /// -> arguments
  ..args,
) = {
  assert(
    args.named().len() == 0,
    message: "unexpected arguments: " + repr(arguments(..args.named())),
  )
  let unioned = args
    .pos()
    .map(spec => result-unwrap(spec-parse(spec)))
    .fold(empty, union2)
  unioned.name = __name__
  unioned
}

/// Builds an array spec.
///
/// - `inner`: Spec for each array item.
/// -> spec
#let array(
  /// Optional display name.
  /// -> str | auto
  __name__: auto,
  /// Item spec.
  /// -> spec
  inner,
) = {
  result-unwrap(result-map(
    inner => (
      __tag__: "spec/array",
      name: __name__,
      inner: inner,
    ),
    spec-parse(inner),
  ))
}

/// Builds a dictionary spec.
///
/// - `value`: Spec for each value.
/// -> spec
#let dictionary(
  /// Optional display name.
  /// -> str | auto
  __name__: auto,
  /// Value spec.
  /// -> spec
  value,
) = {
  result-unwrap(result-map(
    value => (
      __tag__: "spec/dictionary",
      name: __name__,
      value: value,
    ),
    spec-parse(value),
  ))
}

/// Backwards-compatible shorthand for `dictionary`.
#let dict = dictionary

/// Builds a function spec from domain arguments and a codomain.
///
/// Positional and named domain arguments are passed first. The returned function
/// accepts the codomain spec.
/// -> function
#let fun(
  /// Domain argument specs.
  /// -> arguments
  ..dom,
) = cod => {
  result-unwrap(result-map2(
    (dom, cod) => (
      __tag__: "spec/function",
      name: auto,
      dom: dom,
      cod: cod,
    ),
    args-spec-parse(dom),
    spec-parse(cod),
  ))
}

/// Spec for names used by specs.
/// -> spec
#let SPEC-NAME = union(str, std.type(auto))

/// Spec for function argument specs parameterized by an inner spec.
/// -> spec
#let ARGS-SPEC(
  /// Spec used for argument entries.
  /// -> spec
  T,
) = enum(
  __name__: "args-spec(" + to-string(T) + ")",
  null: none,
  args: (
    pos: array(T),
    named: dictionary(T),
  ),
)

/// Spec for constructor specs parameterized by an inner spec.
/// -> spec
#let CONSTR-SPEC(
  /// Spec used for field entries.
  /// -> spec
  T,
) = enum(
  __name__: "constr-spec(" + to-string(T) + ")",
  null: none,
  fields: (
    fields: dictionary(T),
  ),
)

/// Meta-spec describing valid specs.
///
/// Useful for validating that a value is itself a well-formed spec.
/// -> spec
#let SPEC = fix(
  __name__: "spec",
  self => enum(
    __name__: "spec-shape",
    builtin: (name: SPEC-NAME, value: std.type),
    any: none,
    enum: (name: SPEC-NAME, constrs: dictionary(CONSTR-SPEC(self))),
    struct: (name: SPEC-NAME, fields: dictionary(self)),
    union: (name: SPEC-NAME, elems: array(self)),
    array: (name: SPEC-NAME, inner: self),
    dictionary: (name: SPEC-NAME, value: self),
    function: (name: SPEC-NAME, dom: ARGS-SPEC(self), cod: self),
    fix: (name: str, fun: fun(self)(self)),
    self: (depth: int),
  ),
)

/// Structured context attached to validation errors.
///
/// Each frame records the outer validation step and points to the more specific
/// error through `cont`.
/// -> spec
#let TRACE = fix(
  __name__: "trace",
  self => enum(
    __name__: "trace-shape",
    root: none,
    val: (spec: SPEC, value: any, cont: self),
    constr-null: (constr-spec: CONSTR-SPEC(SPEC), value: arguments),
    constr-field: (
      constr-spec: CONSTR-SPEC(SPEC),
      constr-arg: str,
      value: any,
      cont: self,
    ),
    constr: (name: str, cont: self),
    constr-missing-arg: (
      constr-spec: CONSTR-SPEC(SPEC),
      constr-arg: str,
    ),
    constr-extra-args: (
      constr-spec: CONSTR-SPEC(SPEC),
      extra-args: arguments,
    ),
    constr-missing-field: (
      constr-spec: CONSTR-SPEC(SPEC),
      constr-arg: str,
    ),
    args-null: (extra-args: arguments),
    args-arity: (
      args-spec: ARGS-SPEC(SPEC),
      value: arguments,
    ),
    args-extra-named: (
      args-spec: ARGS-SPEC(SPEC),
      extra-args: dictionary(any),
    ),
    args-missing-named: (
      args-spec: ARGS-SPEC(SPEC),
      missing-args: array(str),
    ),
    args-pos-arg: (
      args-spec: ARGS-SPEC(SPEC),
      index: int,
      value: any,
      cont: self,
    ),
    args-named-arg: (
      args-spec: ARGS-SPEC(SPEC),
      name: str,
      value: any,
      cont: self,
    ),
    array-val: (index: int, value: any, cont: self),
    dictionary-val: (key: str, value: any, cont: self),
    union: (spec: SPEC, value: any, cont: self),
  ),
)
