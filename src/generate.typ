#import "bootstrap.typ": *
#import "methods/intro.typ": *
#import "methods/elim.typ": *
#import "methods/fields.typ": *
#import "methods/rec.typ": *
#import "methods/annotate.typ": *

/// Generates all helpers available for a spec.
///
/// The returned dictionary may contain `intro`, `intros`, `elim`, `fields`,
/// `rec`, and `annotate`, depending on the spec kind.
/// -> dictionary
#let generate(spec) = (:
  ..generate-intro(spec),
  ..generate-elim(spec),
  ..generate-fields(spec),
  ..generate-rec(spec),
  ..generate-annotate(spec),
)
