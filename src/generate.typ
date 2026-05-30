#import "bootstrap.typ": *
#import "methods/intro.typ": *
#import "methods/elim.typ": *
#import "methods/is.typ": *
#import "methods/fields.typ": *
#import "methods/rec.typ": *
#import "methods/annotate.typ": *
#import "methods/repr.typ": *

/// Generates all helpers available for a spec.
///
/// The returned dictionary may contain `intro`, `intros`, `elim`, `fields`,
/// `rec`, and `annotate`, depending on the spec kind.
/// -> dictionary
#let generate(spec) = (
  validate: validate.with(spec),
  repr: repr.with(spec),
  ..generate-intro(spec),
  ..generate-elim(spec),
  ..generate-is(spec),
  ..generate-fields(spec),
  ..generate-rec(spec),
  ..generate-annotate(spec),
)
