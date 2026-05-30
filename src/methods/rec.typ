#import "../bootstrap.typ": *
#import "../validate.typ": *
#import "../std.typ" as std
#import "common.typ": *

/// Recurses into fields that point back to a fixed-point spec.
/// -> any
#let generate-rec-field(go, field-spec, value) = {
  if spec-is-fix(field-spec) {
    go(value)
  } else {
    value
  }
}

/// Projects fields for recursive folds.
///
/// Missing fields are skipped so a fold over an annotated target spec can
/// synthesize new annotation fields from a partially annotated input value.
/// -> RESULT(dictionary)
#let project-constr-rec(constr-spec, value) = constr-spec-elim(
  null: ok((:)),
  fields: field-specs => {
    let fields = (:)
    for (field-name, field-spec) in field-specs.pairs() {
      if value.keys().contains(field-name) {
        if spec-is-fix(field-spec) {
          fields.insert(field-name, value.at(field-name))
        } else {
          let result = validate(field-spec, value.at(field-name))
          if result-is-err(result) {
            return result-trace(
              cont => (
                __tag__: "trace/constr-field",
                constr-spec: constr-spec,
                constr-arg: field-name,
                value: value.at(field-name),
                cont: cont,
              ),
              result,
            )
          }
          fields.insert(field-name, result.value)
        }
      }
    }
    ok(fields)
  },
)(constr-spec)

/// Finds non-spec fields preserved across recursive transforms.
///
/// Used to keep annotations and other extra fields during folds.
/// -> dictionary
#let extra-value-fields(constr-spec, value) = {
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
#let merge-extra-fields(value, extras) = {
  if std.type(value) != std.dictionary {
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
#let rebuild-with-extra-fields(spec, tag, fields, extras, value) = {
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
#let generate-enum-rec(spec, constrs) = (
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
      if std.type(value) != std.dictionary or not value.keys().contains("__tag__") {
        panic("not an enum value", value)
      }
      let tag = value.remove("__tag__").split("/").last()
      if not constrs.keys().contains(tag) {
        panic("unknown constructor `" + tag + "`")
      }
      let constr-spec = constrs.at(tag)
      let fields = pretty-result-unwrap(project-constr-rec(constr-spec, value))
      let extras = extra-value-fields(constr-spec, value)
      let finish(result) = {
        if std.type(result) == std.dictionary {
          merge-extra-fields(result, extras)
        } else {
          rebuild-with-extra-fields(spec, tag, fields, extras, result)
        }
      }
      let case = cases.at(tag)
      if std.type(case) != std.function {
        return finish(case)
      }
      let result = if constr-spec.__tag__ == "constr-spec/null" {
        case()
      } else if constr-spec.__tag__ == "constr-spec/fields" {
        let mapped = (:)
        for (field-name, field-spec) in constr-spec.fields.pairs() {
          if fields.keys().contains(field-name) {
            mapped.insert(field-name, generate-rec-field(
              go,
              field-spec,
              fields.at(field-name),
            ))
          }
        }
        case(..mapped.values())
      } else {
        panic("ill-formed constructor spec: `" + repr(constr-spec) + "`")
      }
      if std.type(result) == std.function {
        (..args) => finish(result(..args))
      } else {
        finish(result)
      }
    }
    go
  },
)

/// Generates recursive folds for recursive enum specs.
/// -> dictionary
#let generate-rec(spec) = spec-elim(
  fix: (name, fun) => spec-elim(
    enum: (name, constrs) => generate-enum-rec(spec, constrs),
    __default__: (:)
  )(fun(spec)),
  __default__: (..args) => (:),
)(spec)

/// Builds a recursive fold directly from a spec and cases.
/// -> function
#let rec(spec, ..cases) = {
  let generated = generate-rec(spec)
  if not generated.keys().contains("rec") {
    panic("spec does not support recursive folds")
  }
  (generated.rec)(..cases)
}
