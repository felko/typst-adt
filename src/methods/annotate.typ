#import "../bootstrap.typ": *
#import "../validate.typ": *
#import "../std.typ" as std
#import "common.typ": *

/// Builds recursive annotation for enum specs.
///
/// Each case returns the annotation fields for that constructor.
/// -> dictionary(str, function)
#let generate-enum-annotate(spec, constrs) = (
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
      std.type(ann-specs) == std.dictionary,
      message: "expected `__ann__` to be a dictionary",
    )
    ann-specs = pretty-result-unwrap(result-all-dict(spec-parse, ann-specs))
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
      if (
        std.type(value) != std.dictionary
          or not value.keys().contains("__tag__")
      ) {
        panic("not an enum value", value)
      }
      let tag = value.remove("__tag__").split("/").last()
      if not constrs.keys().contains(tag) {
        panic("unknown constructor `" + tag + "`")
      }
      let constr-spec = constrs.at(tag)
      let fields = pretty-result-unwrap(project-constr(constr-spec, value))
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
      let ann-value = if std.type(case) == std.function {
        case(..algebra-fields.values())
      } else {
        case
      }
      ann-value = pretty-result-unwrap(validate(
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
#let generate-annotate(spec) = spec-elim(
  enum: (name, constrs) => generate-enum-annotate(spec, constrs),
  fix: (name, fun) => spec-elim(
    enum: (name, constrs) => generate-enum-annotate(spec, constrs),
    __default__: (:),
  )(fun(spec)),
  __default__: (:),
)(spec)
