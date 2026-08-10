// Prototype de "shallow validation" pour les champs auto-recursifs (fix/self).
//
// Contexte : project-constr-args (methods/common.typ) ne valide PAS DU TOUT
// un champ dont le spec contient self/fix (spec-contains-self), pour eviter
// une recursion infinie sur des types comme Commit.parent1: PARENT(self).
// Consequence : on peut passer n'importe quelle valeur (une string !) a la
// place d'un vrai sous-Commit, sans jamais que ce soit detecte.
//
// Ce prototype explore une alternative : au lieu de "rien valider", on
// valide seulement jusqu'a une profondeur donnee (verifier le tag + la
// forme immediate), puis on remplace ce qui est plus profond et recursif
// par `adt.any`. Le but est de retrouver une garantie minimale (le tag est
// bon, les champs immediats existent et ont le bon type) sans re-descendre
// dans toute la chaine recursive (ce qui recreerait le depassement de
// profondeur d'appel).
//
// Ecrit en important les fichiers source de adt directement (bootstrap.typ,
// spec.typ, validate.typ) plutot que le lib.typ public, car spec-elim /
// constr-spec-elim / spec-contains-self ne sont pas exposes publiquement.
// Rien n'est modifie dans src/ : ce fichier ne fait qu'importer.

#import "../../src/lib.typ" as adt
#import "../../src/bootstrap.typ": spec-elim, constr-spec-elim
#import "../../src/spec.typ": any, spec-parse
#import "../../src/result.typ": ok, err, result-is-err, result-is-ok
#import "../../src/validate.typ": validate

// ----------------------------------------------------------------------
// spec-contains-self : copie conforme de methods/common.typ (deja en
// production), reproduite ici pour le prototype.
// ----------------------------------------------------------------------
#let spec-contains-self(spec) = spec-elim(
  builtin: type_ => false,
  any: () => false,
  enum: (name, constrs) => constrs.values().any(cs =>
    constr-spec-elim(
      null: false,
      fields: fields => fields.values().any(spec-contains-self),
    )(cs)
  ),
  struct: (name, fields) => fields.values().any(spec-contains-self),
  union: (name, elems) => elems.any(spec-contains-self),
  array: (name, inner) => spec-contains-self(inner),
  dictionary: (name, value) => spec-contains-self(value),
  function: (name, dom, cod) => false,
  fix: (name, fun) => true,
  self: depth => true,
)(spec)

// ----------------------------------------------------------------------
// spec-shallow(spec, depth) : deplie les `fix` sans jamais les compter
// comme un "niveau" (un fix decrit juste "ce type est recursif", ce n'est
// pas en soi le point de coupure), mais decremente `depth` a chaque
// niveau struct/enum reellement traverse. Des que depth <= 0 ET qu'un
// champ contient encore un self non resolu, ce champ devient `any`.
// ----------------------------------------------------------------------
#let spec-shallow(spec, depth: 0) = spec-elim(
  builtin: type_ => spec,
  any: () => spec,
  enum: (name, constrs) => (
    __tag__: "spec/enum",
    name: name,
    constrs: constrs.pairs().map(((constr-name, cs)) => (
      constr-name,
      constr-spec-elim(
        null: (__tag__: "constr-spec/null"),
        fields: field-specs => (
          __tag__: "constr-spec/fields",
          fields: field-specs.pairs().map(((fname, fspec)) => (
            fname,
            if depth <= 0 and spec-contains-self(fspec) {
              any
            } else {
              spec-shallow(fspec, depth: depth - 1)
            },
          )).to-dict(),
        ),
      )(cs),
    )).to-dict(),
  ),
  struct: (name, fields) => (
    __tag__: "spec/struct",
    name: name,
    fields: fields.pairs().map(((fname, fspec)) => (
      fname,
      if depth <= 0 and spec-contains-self(fspec) {
        any
      } else {
        spec-shallow(fspec, depth: depth - 1)
      },
    )).to-dict(),
  ),
  union: (name, elems) => spec,
  array: (name, inner) => spec,
  dictionary: (name, value) => spec,
  function: (name, dom, cod) => spec,
  fix: (name, fun) => spec-shallow(fun(spec), depth: depth),
  self: depth_ => any,
)(spec)

#repr("spec-shallow / spec-contains-self definies OK")
#text("\n\n")

// ========================================================================
// CAS DE TEST 1 : type recursif minimal equivalent a PARENT(self)/Commit,
// verifier que spec-shallow(depth: 0) rejette une valeur dont le champ
// recursif immediat n'a pas la bonne forme.
// ========================================================================

#let LEAF = adt.struct(__name__: "Leaf", name: str)
#let (intro: leaf) = adt.generate(LEAF)

#let TREEISH = adt.fix(
  __name__: "Treeish",
  self => adt.enum(
    __name__: "Treeish",
    leaf: LEAF,
    node: (left: self, right: self),
  ),
)
#let (intro: (leaf: tree-leaf, node: tree-node)) = adt.generate(TREEISH)

#let good-tree = tree-node(tree-leaf(leaf("a")), tree-leaf(leaf("b")))
#repr(good-tree)
#text("\n\nvalidation de good-tree avec spec-shallow(depth:0) -- doit REUSSIR :\n")
#let good-result = validate(spec-shallow(TREEISH, depth: 0), good-tree)
#repr(good-result)
#text("\n\nresult-is-ok(good-result) = ")
#repr(result-is-ok(good-result))
#text("\n\n")

#let garbage-tree = (__tag__: "node", left: "ceci n'est pas un Treeish", right: tree-leaf(leaf("b")))
#repr(garbage-tree)
#text("\n\nvalidation de garbage-tree avec spec-shallow(depth:0) -- doit ECHOUER :\n")
#let garbage-result = validate(spec-shallow(TREEISH, depth: 0), garbage-tree)
#repr(garbage-result)
#text("\n\nresult-is-err(garbage-result) = ")
#repr(result-is-err(garbage-result))
#text("\n\n=== CAS DE TEST 1 termine ===\n\n")

// ========================================================================
// CAS DE TEST 2 : un arbre PROFOND (beaucoup de node imbriques), pour
// verifier que spec-shallow(depth: 0) ne re-explose PAS la pile d'appel
// (c'est tout le but : ne verifier qu'un seul niveau, faire confiance au
// reste).
// ========================================================================

// Construction ITERATIVE (boucle for, pas d'appel de fonction recursif)
// pour ne pas consommer nous-memes du budget de pile Typst avant meme
// d'avoir teste validate/spec-shallow.
#let big-tree = {
  let t = tree-leaf(leaf("bottom"))
  for n in range(60) {
    t = tree-node(t, tree-leaf(leaf(str(n))))
  }
  t
}
#text("arbre profond (60 niveaux, construit iterativement) OK\n\n")
#let big-result = validate(spec-shallow(TREEISH, depth: 0), big-tree)
#text("validation shallow d'un arbre profond -- doit REUSSIR sans exploser la pile :\n")
#repr(result-is-ok(big-result))
#text("\n\n=== CAS DE TEST 2 termine ===\n\n")

// ========================================================================
// CAS DE TEST 3 : comparaison avec validate() PLEIN (sans shallow) sur le
// meme arbre profond, pour confirmer que c'est bien LUI qui explose (et
// donc que spec-shallow apporte un vrai gain), pas juste une coincidence.
// Ce bloc est COMMENTE par defaut : decommenter pour verifier qu'il
// plante bien avec "maximum function call depth exceeded", PUIS
// recommenter avant de continuer (sinon plus rien ne compile apres).
// ========================================================================

#text("validation PLEINE (non-shallow) d'un arbre profond -- doit CRASHER :\n")
//#repr(result-is-ok(validate(TREEISH, big-tree)))
// Confirme : decommenter les 2 lignes ci-dessus reproduit bien
// "maximum function call depth exceeded" avec le meme pattern
// validate-constr-aux <-> validate que le bug original (portfolio).
