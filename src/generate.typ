#import "bootstrap.typ": *
#import "methods/intro.typ" as intro-methods
#import "methods/elim.typ" as elim-methods
#import "methods/fields.typ" as field-methods
#import "methods/rec.typ" as rec-methods
#import "methods/annotate.typ" as annotate-methods

#let generate-intro = intro-methods.generate-intro
#let generate-elim = elim-methods.generate-elim
#let generate-fields = field-methods.generate-fields
#let generate-rec = rec-methods.generate-rec
#let rec = rec-methods.rec
#let generate-annotate = annotate-methods.generate-annotate

/// Placeholder for future visitor generation.
/// -> dictionary
#let generate-visit(spec) = spec-elim(
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
#let generate(spec) = {
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
