#import "bootstrap.typ": *
#import "validate.typ": *

/// Returns whether a spec is a fixed-point spec.
/// -> bool
#let spec-is-fix(
  /// Spec to inspect.
  /// -> spec
  spec,
) = spec-elim(
  empty_case: () => false,
  builtin: type_ => false,
  any: () => false,
  union_case: (name, elems) => false,
  enum: (name, constrs) => false,
  struct: (name, fields) => false,
  array_case: (name, inner) => false,
  dictionary_case: (name, key, value) => false,
  function_case: (name, dom, cod) => false,
  fix: (name, fun) => true,
  self: depth => false,
)(spec)

/// Projects constructor arguments into validated fields.
///
/// Recursive fields are left as-is so recursive values can be projected without
/// forcing validation of the whole recursive shape.
/// -> RESULT(dictionary)
#let project-constr-args(
  /// Constructor spec.
  /// -> constr-spec
  constr-spec,
  /// Constructor arguments.
  /// -> arguments
  ..args
) = constr-spec-elim(
  none_: {
    let named = args.named()
    if args.pos().len() != 0 or named.len() != 0 {
      err("expected no arguments, got `" + repr(args) + "`")
    } else {
      ok((:))
    }
  },
  fields: field-specs => {
    let (pos, named) = (args.pos(), args.named())
    let fields = (:)
    for (field-name, field-spec) in field-specs.pairs() {
      let arg = if named.keys().contains(field-name) {
        named.remove(field-name)
      } else if pos.len() == 0 {
        return err("not enough arguments: `" + repr(args) + "`")
      } else {
        let (arg, ..new-pos) = pos
        pos = new-pos
        arg
      }
      if spec-is-fix(field-spec) {
        fields.insert(field-name, arg)
      } else {
        let result = validate(field-spec, arg)
        if result-is-err(result) {
          return result
        }
        fields.insert(field-name, result.value)
      }
    }
    if pos.len() > 0 or named.len() > 0 {
      err("unrecognizd arguments: `" + repr(arguments(..pos, ..named)) + "`")
    } else {
      ok(fields)
    }
  },
)(constr-spec)

/// Builds a raw constructor for a constructor spec.
///
/// Prefer generated constructors from `generate` for user-facing values.
/// -> function | dictionary
#let generate-constr(
  /// Constructor tag, or `none` for structs.
  /// -> str | none
  tag,
  /// Constructor spec.
  /// -> constr-spec
  constr-spec,
) = constr-spec-elim(
  none_: if tag == none { (:) } else { (__tag__: tag) },
  fields: _ => {
    if tag == none {
      (..args) => result-unwrap(validate-constr(constr-spec, ..args))
    } else {
      (..args) => (
        __tag__: tag,
        ..result-unwrap(validate-constr(constr-spec, ..args)),
      )
    }
  },
)(constr-spec)

/// Builds a constructor for a generated spec.
///
/// Generated constructors return plain values and do not attach methods.
/// -> function | dictionary
#let generate-constr-with-spec(
  /// Constructor tag, or `none` for structs.
  /// -> str | none
  tag,
  /// Constructor spec.
  /// -> constr-spec
  constr-spec,
) = constr-spec-elim(
  none_: if tag == none { (:) } else { (__tag__: tag) },
  fields: _ => {
    if tag == none {
      (..args) => result-unwrap(project-constr-args(constr-spec, ..args))
    } else {
      (..args) => (
        __tag__: tag,
        ..result-unwrap(project-constr-args(constr-spec, ..args)),
      )
    }
  },
)(constr-spec)

/// Builds an intro function for plain validated values.
///
/// The returned function validates input and returns the plain validated value.
/// -> function
#let generate-value-intro(
  /// Spec to validate against.
  /// -> spec
  spec,
) = value => result-unwrap(validate(spec, value))

/// Builds an eliminator for plain validated values.
///
/// The eliminator validates the value before applying the provided function.
/// -> function
#let generate-value-elim(
  /// Spec to validate against.
  /// -> spec
  spec,
) = f => value => {
  let value = result-unwrap(validate(spec, value))
  if type(f) == function {
    f(value)
  } else {
    f
  }
}

/// Wraps a function with argument and return validation.
///
/// The wrapped function validates call arguments and the return value.
/// -> function
#let generate-function-intro(
  /// Domain argument spec.
  /// -> args-spec
  dom,
  /// Codomain spec.
  /// -> spec
  cod,
) = f => {
  assert(
    type(f) == function,
    message: "expected function, got `" + str(type(f)) + "`",
  )
  (..args) => {
    let args = result-unwrap(validate-args(dom, ..args))
    result-unwrap(validate(cod, f(..args)))
  }
}

/// Builds a validated function application eliminator.
///
/// The returned function validates the function value, arguments, and result.
/// -> function
#let generate-function-elim(
  /// Domain argument spec.
  /// -> args-spec
  dom,
  /// Codomain spec.
  /// -> spec
  cod,
) = value => (..args) => {
  let value = result-unwrap(validate(
    (__tag__: "spec/function", name: auto, dom: dom, cod: cod),
    value,
  ))
  let args = result-unwrap(validate-args(dom, ..args))
  result-unwrap(validate(cod, value(..args)))
}

/// Projects validated fields out of a constructor value.
///
/// Extra fields are ignored.
/// -> RESULT(dictionary)
#let project-constr(
  /// Constructor spec.
  /// -> constr-spec
  constr-spec,
  /// Constructor value.
  /// -> dictionary
  value,
) = constr-spec-elim(
  none_: ok((:)),
  fields: field-specs => {
    let fields = (:)
    for (field-name, field-spec) in field-specs.pairs() {
      if not value.keys().contains(field-name) {
        return err("missing field `" + field-name + "`")
      }
      if spec-is-fix(field-spec) {
        fields.insert(field-name, value.at(field-name))
      } else {
        let result = validate(field-spec, value.at(field-name))
        if result-is-err(result) {
          return result
        }
        fields.insert(field-name, result.value)
      }
    }
    ok(fields)
  },
)(constr-spec)

/// Recurses into fields that point back to a fixed-point spec.
/// -> any
#let generate-rec-field(
  /// Recursive function.
  /// -> function
  go,
  /// Field spec.
  /// -> spec
  field-spec,
  /// Field value.
  /// -> any
  value,
) = {
  if spec-is-fix(field-spec) {
    go(value)
  } else {
    value
  }
}

/// Finds non-spec fields preserved across recursive transforms.
///
/// Used to keep annotations and other extra fields during folds.
/// -> dictionary
#let extra-value-fields(
  /// Constructor spec.
  /// -> constr-spec
  constr-spec,
  /// Value to inspect.
  /// -> dictionary
  value,
) = {
  let extras = value
  let _ = extras.remove("__tag__", default: none)
  if constr-spec.__tag__ == "constr-spec/fields" {
    for field-name in constr-spec.fields.keys() {
      let _ = extras.remove(field-name, default: none)
    }
  }
  extras
}

/// Adds preserved extra fields back to a dictionary value.
/// -> any
#let merge-extra-fields(
  /// Value to receive fields.
  /// -> any
  value,
  /// Extra fields.
  /// -> dictionary
  extras,
) = {
  if type(value) != dictionary {
    return value
  }
  for (field-name, field-value) in extras.pairs() {
    if not value.keys().contains(field-name) {
      value.insert(field-name, field-value)
    }
  }
  value
}

/// Rebuilds a value when recursive folding returns a plain result.
///
/// Preserved extra fields are merged back onto the rebuilt value.
/// -> any
#let rebuild-with-extra-fields(
  /// Parent spec.
  /// -> spec
  spec,
  /// Constructor tag.
  /// -> str
  tag,
  /// Constructor fields.
  /// -> dictionary
  fields,
  /// Extra fields to preserve.
  /// -> dictionary
  extras,
  /// Fold result.
  /// -> any
  value,
) = {
  if extras.len() == 0 {
    return value
  }
  let rebuilt = (__tag__: tag, ..fields)
  for (field-name, field-value) in extras.pairs() {
    rebuilt.insert(field-name, field-value)
  }
  if not rebuilt.keys().contains("depth") {
    rebuilt.insert("depth", value)
  }
  rebuilt
}

/// Builds recursive folds for enum specs.
///
/// The generated `rec` function recursively maps self fields before calling
/// each constructor case.
/// -> dictionary(str, function)
#let generate-enum-rec(
  /// Recursive enum spec.
  /// -> spec
  spec,
  /// Enum constructors.
  /// -> dictionary(str, constr-spec)
  constrs,
) = (
  rec: (..cases) => {
    assert(
      cases.pos().len() == 0,
      message: "expected no positional arguments, got " + repr(cases.pos()),
    )
    cases = cases.named()
    assert(
      cases.keys().all(k => constrs.keys().contains(k)),
      message: "unrecognized cases: "
        + cases
          .keys()
          .filter(k => not constrs.keys().contains(k))
          .map(k => "`" + k + "`")
          .join(", "),
    )
    assert(
      constrs.keys().all(k => cases.keys().contains(k)),
      message: "missing cases: "
        + constrs
          .keys()
          .filter(k => not cases.keys().contains(k))
          .map(k => "`" + k + "`")
          .join(", "),
    )
    let go(value) = {
      if type(value) != dictionary or not value.keys().contains("__tag__") {
        panic("not an enum value: `" + repr(value) + "`")
      }
      let tag = value.remove("__tag__").split("/").last()
      if not constrs.keys().contains(tag) {
        panic("unknown constructor `" + tag + "`")
      }
      let constr-spec = constrs.at(tag)
      let fields = result-unwrap(project-constr(constr-spec, value))
      let extras = extra-value-fields(constr-spec, value)
      let finish(result) = {
        if type(result) == dictionary {
          merge-extra-fields(result, extras)
        } else {
          rebuild-with-extra-fields(spec, tag, fields, extras, result)
        }
      }
      let case = cases.at(tag)
      if type(case) != function {
        return finish(case)
      }
      let result = if constr-spec.__tag__ == "constr-spec/none" {
        case()
      } else if constr-spec.__tag__ == "constr-spec/fields" {
        let mapped = (:)
        for (field-name, field-spec) in constr-spec.fields.pairs() {
          mapped.insert(field-name, generate-rec-field(
            go,
            field-spec,
            fields.at(field-name),
          ))
        }
        case(..mapped.values())
      } else {
        panic("ill-formed constructor spec: `" + repr(constr-spec) + "`")
      }
      if type(result) == function {
        (..args) => finish(result(..args))
      } else {
        finish(result)
      }
    }
    go
  },
)

/// Builds constructor functions for every enum constructor.
/// -> dictionary(str, function)
#let generate-enum-intros(
  /// Enum spec.
  /// -> spec
  spec,
  /// Enum constructors.
  /// -> dictionary(str, constr-spec)
  constrs,
) = (
  constrs
    .pairs()
    .map(((constr-name, constr-spec)) => {
      (
        constr-name,
        generate-constr-with-spec(constr-name, constr-spec),
      )
    })
    .to-dict()
)

/// Generates intro helpers for a spec.
///
/// Returns `intro` for most specs and both `intro`/`intros` for enums.
/// -> dictionary
#let generate-intro(
  /// Spec to generate for.
  /// -> spec
  spec,
) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (intro: generate-value-intro(spec)),
  any: () => (intro: value => value),
  union_case: (name, elems) => (intro: generate-value-intro(spec)),
  struct: (name, fields) => (
    intro: generate-constr-with-spec(
      none,
      (__tag__: "constr-spec/fields", fields: fields),
    ),
  ),
  enum: (name, constrs) => {
    let intros = generate-enum-intros(spec, constrs)
    (intro: intros, intros: intros)
  },
  array_case: (name, inner) => (intro: generate-value-intro(spec)),
  dictionary_case: (name, key, inner) => (
    intro: generate-value-intro(spec),
  ),
  function_case: (name, dom, cod) => (intro: generate-function-intro(dom, cod)),
  fix: (name, fun) => spec-elim(
    empty_case: () => (:),
    builtin: type_ => (:),
    any: () => (:),
    union_case: (name, elems) => (:),
    struct: (name, fields) => (
      intro: generate-constr-with-spec(
        none,
        (__tag__: "constr-spec/fields", fields: fields),
      ),
    ),
    enum: (name, constrs) => {
      let intros = generate-enum-intros(spec, constrs)
      (intro: intros, intros: intros)
    },
    array_case: (name, inner) => (:),
    dictionary_case: (name, key, value) => (:),
    function_case: (name, dom, cod) => (:),
    fix: (name, fun) => (:),
    self: (..args) => (:),
  )(fun(spec)),
  self: (..args) => (:),
)(spec)

/// Converts a constructor spec to function arguments.
/// -> arguments
#let constr-spec-args(
  /// Constructor spec.
  /// -> constr-spec
  constr-spec,
) = constr-spec-elim(
  none_: arguments(),
  fields: fields => arguments(..fields),
)(constr-spec)

/// Builds the expected case-function spec for an eliminator.
///
/// `T` is the result spec shared by every case function.
/// -> spec
#let CASES(
  /// Spec to build cases for.
  /// -> spec
  spec,
  /// Result spec for each case.
  /// -> spec
  T,
) = spec-struct(
  ..spec-elim(
    empty_case: () => panic("empty specs do not have cases"),
    builtin: type_ => panic("builtin specs do not have cases"),
    any: () => panic("any specs do not have cases"),
    union_case: (name, elems) => panic("union specs do not have cases"),
    enum: (name, constrs) => constrs
      .pairs()
      .map(((constr-name, constr-spec)) => {
        (constr-name, spec-function(..constr-spec-args(constr-spec))(T))
      })
      .to-dict(),
    struct: (name, fields) => (mk: spec-function(..fields)(T)),
    array_case: (name, inner) => panic("array specs do not have cases"),
    dictionary_case: (name, key, value) => panic(
      "dictionary specs do not have cases",
    ),
    function_case: (name, dom, cod) => panic(
      "function specs do not have cases",
    ),
    fix: (name, fun) => CASES(fun(spec), T),
    self: depth => panic("self specs do not have cases"),
  )(spec),
)

/// Generates eliminators for a spec.
///
/// Enum eliminators require one case per constructor.
/// -> dictionary
#let generate-elim(
  /// Spec to generate for.
  /// -> spec
  spec,
) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (elim: generate-value-elim(spec)),
  struct: (name, fields) => (
    elim: mk => value => {
      let fields = result-unwrap(validate(spec, value))
      if type(mk) == function {
        mk(..fields.values())
      } else {
        mk
      }
    },
  ),
  any: () => (elim: f => x => f(x)),
  union_case: (name, elems) => (elim: generate-value-elim(spec)),
  enum: (name, constrs) => (
    elim: (..cases) => {
      assert(
        cases.pos().len() == 0,
        message: "expected no positional arguments, got " + repr(cases.pos()),
      )
      cases = cases.named()
      assert(
        cases.keys().all(k => constrs.keys().contains(k)),
        message: "unrecognized cases: "
          + cases
            .keys()
            .filter(k => not constrs.keys().contains(k))
            .map(k => "`" + k + "`")
            .join(", "),
      )
      assert(
        constrs.keys().all(k => cases.keys().contains(k)),
        message: "missing cases: "
          + constrs
            .keys()
            .filter(k => not cases.keys().contains(k))
            .map(k => "`" + k + "`")
            .join(", "),
      )
      value => {
        if type(value) == dictionary and value.keys().contains("__tag__") {
          let tag = value.remove("__tag__").split("/").last()
          let args = result-unwrap(project-constr(spec.constrs.at(tag), value))
          let case = cases.at(tag)
          if type(case) == function {
            case(..args.values())
          } else {
            case
          }
        } else {
          panic("not an enum value: `" + repr(value) + "`")
        }
      }
    },
  ),
  array_case: (name, inner) => (elim: generate-value-elim(spec)),
  dictionary_case: (name, key, inner) => (elim: generate-value-elim(spec)),
  function_case: (name, dom, cod) => (elim: generate-function-elim(dom, cod)),
  fix: (name, fun) => generate-elim(fun(spec)),
  self: (..args) => (:),
)(spec)

/// Builds a field accessor for enum values.
///
/// Panics when the current constructor does not provide the requested field.
/// -> function
#let generate-enum-field(
  /// Enum spec.
  /// -> spec
  spec,
  /// Enum constructors.
  /// -> dictionary(str, constr-spec)
  constrs,
  /// Field name to access.
  /// -> str
  field-name,
) = value => {
  if type(value) != dictionary or not value.keys().contains("__tag__") {
    panic("not an enum value: `" + repr(value) + "`")
  }
  let tag = value.remove("__tag__").split("/").last()
  if not constrs.keys().contains(tag) {
    panic("unknown constructor `" + tag + "`")
  }
  let constr-spec = constrs.at(tag)
  let fields = result-unwrap(project-constr(constr-spec, value))
  if fields.keys().contains(field-name) {
    fields.at(field-name)
  } else {
    panic("constructor `" + tag + "` does not have field `" + field-name + "`")
  }
}

/// Builds field accessors shared across enum constructors.
///
/// Accessors are generated for the union of all constructor field names.
/// -> dictionary
#let generate-enum-fields(
  /// Enum spec.
  /// -> spec
  spec,
  /// Enum constructors.
  /// -> dictionary(str, constr-spec)
  constrs,
) = {
  let field-names = ()
  for constr-spec in constrs.values() {
    if constr-spec.__tag__ == "constr-spec/fields" {
      field-names += constr-spec.fields.keys()
    }
  }
  (
    fields: field-names
      .dedup()
      .map(field-name => (
        field-name,
        generate-enum-field(spec, constrs, field-name),
      ))
      .to-dict(),
  )
}

/// Generates field accessors for structs and enums.
/// -> dictionary
#let generate-fields(
  /// Spec to generate for.
  /// -> spec
  spec,
) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (
    fields: fields
      .keys()
      .map(field-name => (
        field-name,
        value => result-unwrap(validate(spec, value)).at(field-name),
      ))
      .to-dict(),
  ),
  enum: (name, constrs) => generate-enum-fields(spec, constrs),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
  fix: (name, fun) => generate-fields(fun(spec)),
  self: (..args) => (:),
)(spec)

/// Generates recursive folds for recursive enum specs.
/// -> dictionary
#let generate-rec(
  /// Spec to generate for.
  /// -> spec
  spec,
) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => (:),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
  fix: (name, fun) => spec-elim(
    empty_case: () => (:),
    builtin: type_ => (:),
    any: () => (:),
    union_case: (name, elems) => (:),
    struct: (name, fields) => (:),
    enum: (name, constrs) => generate-enum-rec(spec, constrs),
    array_case: (name, inner) => (:),
    dictionary_case: (name, key, inner) => (:),
    function_case: (name, dom, cod) => (:),
    fix: (name, fun) => (:),
    self: (..args) => (:),
  )(fun(spec)),
  self: (..args) => (:),
)(spec)

/// Builds recursive annotation for enum specs.
///
/// Each case returns the annotation fields for that constructor.
/// -> dictionary(str, function)
#let generate-enum-annotate(
  /// Enum spec.
  /// -> spec
  spec,
  /// Enum constructors.
  /// -> dictionary(str, constr-spec)
  constrs,
) = (
  annotate: (..args) => {
    assert(
      args.pos().len() == 0,
      message: "expected no positional arguments, got " + repr(args.pos()),
    )
    let named = args.named()
    assert(
      named.keys().contains("__ann__"),
      message: "missing `__ann__` annotation spec",
    )
    let ann-specs = named.remove("__ann__")
    assert(
      type(ann-specs) == dictionary,
      message: "expected `__ann__` to be a dictionary",
    )
    ann-specs = result-unwrap(result-all-dict(spec-parse, ann-specs))
    let cases = named
    assert(
      cases.keys().all(k => constrs.keys().contains(k)),
      message: "unrecognized cases: "
        + cases
          .keys()
          .filter(k => not constrs.keys().contains(k))
          .map(k => "`" + k + "`")
          .join(", "),
    )
    assert(
      constrs.keys().all(k => cases.keys().contains(k)),
      message: "missing cases: "
        + constrs
          .keys()
          .filter(k => not cases.keys().contains(k))
          .map(k => "`" + k + "`")
          .join(", "),
    )
    let go(value) = {
      if type(value) != dictionary or not value.keys().contains("__tag__") {
        panic("not an enum value: `" + repr(value) + "`")
      }
      let tag = value.remove("__tag__").split("/").last()
      if not constrs.keys().contains(tag) {
        panic("unknown constructor `" + tag + "`")
      }
      let constr-spec = constrs.at(tag)
      let fields = result-unwrap(project-constr(constr-spec, value))
      let rebuilt = (__tag__: tag)
      let algebra-fields = (:)
      if constr-spec.__tag__ == "constr-spec/fields" {
        for (field-name, field-spec) in constr-spec.fields.pairs() {
          let field-value = fields.at(field-name)
          if spec-is-fix(field-spec) {
            let annotated-child = go(field-value)
            rebuilt.insert(field-name, annotated-child)
            algebra-fields.insert(
              field-name,
              ann-specs
                .keys()
                .map(ann-name => (ann-name, annotated-child.at(ann-name)))
                .to-dict(),
            )
          } else {
            rebuilt.insert(field-name, field-value)
            algebra-fields.insert(field-name, field-value)
          }
        }
      }
      let case = cases.at(tag)
      let ann-value = if type(case) == function {
        case(..algebra-fields.values())
      } else {
        case
      }
      ann-value = result-unwrap(validate(
        (__tag__: "spec/struct", name: auto, fields: ann-specs),
        ann-value,
      ))
      for (ann-name, ann-field-value) in ann-value.pairs() {
        rebuilt.insert(ann-name, ann-field-value)
      }
      rebuilt
    }
    go
  },
)

/// Generates annotation helpers for recursive enum specs.
/// -> dictionary
#let generate-annotate(
  /// Spec to generate for.
  /// -> spec
  spec,
) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => generate-enum-annotate(spec, constrs),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
  fix: (name, fun) => spec-elim(
    empty_case: () => (:),
    builtin: type_ => (:),
    any: () => (:),
    union_case: (name, elems) => (:),
    struct: (name, fields) => (:),
    enum: (name, constrs) => generate-enum-annotate(spec, constrs),
    array_case: (name, inner) => (:),
    dictionary_case: (name, key, inner) => (:),
    function_case: (name, dom, cod) => (:),
    fix: (name, fun) => (:),
    self: (..args) => (:),
  )(fun(spec)),
  self: (..args) => (:),
)(spec)

/// Placeholder for future visitor generation.
/// -> dictionary
#let generate-visit(
  /// Spec to generate for.
  /// -> spec
  spec,
) = spec-elim(
  empty_case: () => (:),
  builtin: type_ => (:),
  any: () => (:),
  union_case: (name, elems) => (:),
  struct: (name, fields) => (:),
  enum: (name, constrs) => (:),
  array_case: (name, inner) => (:),
  dictionary_case: (name, key, inner) => (:),
  function_case: (name, dom, cod) => (:),
  fix: (name, fun) => (:),
  self: (..args) => (:),
)(spec)

/// Generates all helpers available for a spec.
///
/// The returned dictionary may contain `intro`, `intros`, `elim`, `fields`,
/// `rec`, and `annotate`, depending on the spec kind.
/// -> dictionary
#let generate(
  /// Spec to generate helpers for.
  /// -> spec
  spec,
) = {
  let elim = generate-elim(spec)
  let fields = generate-fields(spec)
  let rec = generate-rec(spec)
  let visit = generate-visit(spec)
  let annotate = generate-annotate(spec)
  (:
    ..generate-intro(spec),
    ..elim,
    ..fields,
    ..rec,
    ..annotate,
    ..visit,
  )
}
