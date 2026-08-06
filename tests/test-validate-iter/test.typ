// Tests de la reecriture iterative (defonctionnalisee) de `validate` :
// l'algorithme recursif est remplace par une machine a pile de taches
// (paires spec/valeur + frames d'assemblage + points de choix pour les
// unions), si bien que la profondeur de validation ne consomme plus la
// pile d'appels Typst. Ticket #7 : valider une valeur profondement imbriquee
// (chaine de commits, portfolio dans account/actor/owner) explosait en
// "maximum function call depth exceeded".

#import "../../src/lib.typ" as adt: ok, err, result-is-err, result-is-ok

// ---------------------------------------------------------------------
// 0. SONDE : Typst limite-t-il le nombre d'iterations d'un `while` ?
// La machine repose sur une boucle while ; si cette sonde fait echouer la
// compilation ("loop seems to be infinite"), il faudra chunker la boucle.
// ---------------------------------------------------------------------
#let probe = {
  let i = 0
  while i < 20000 {
    i += 1
  }
  i
}
#assert.eq(probe, 20000)

// ---------------------------------------------------------------------
// 1. Equivalence de comportement sur toutes les formes de spec
// ---------------------------------------------------------------------

// builtin
#assert.eq(adt.validate(int, 3), ok(3))
#assert(result-is-err(adt.validate(int, "x")))
#assert.eq(
  adt.validate(int, "x").msg,
  "expected a value of type `integer`, got `string`",
)

// any
#assert.eq(adt.validate(adt.any, "whatever"), ok("whatever"))

// array
#assert.eq(adt.validate(adt.array(int), (1, 2, 3)), ok((1, 2, 3)))
#assert.eq(adt.validate(adt.array(int), ()), ok(()))
#assert(result-is-err(adt.validate(adt.array(int), (1, "bad"))))
#assert(result-is-err(adt.validate(adt.array(int), "not-an-array")))

// dictionary
#assert.eq(adt.validate(adt.dict(int), (a: 1, b: 2)), ok((a: 1, b: 2)))
#assert(result-is-err(adt.validate(adt.dict(int), (a: 1, b: "bad"))))

// struct : ok, champ manquant, champ en trop, valeur non-dict
#let PAIR = adt.struct(__name__: "PAIR", left: int, right: int)
#assert.eq(adt.validate(PAIR, (left: 1, right: 2)), ok((left: 1, right: 2)))
#assert(result-is-err(adt.validate(PAIR, (left: 1))))
#assert.eq(adt.validate(PAIR, (left: 1)).msg, "not enough arguments")
#assert(result-is-err(adt.validate(PAIR, (left: 1, right: 2, z: 3))))
#assert(result-is-err(adt.validate(PAIR, "bad")))
#assert.eq(
  adt.validate(PAIR, "bad").msg,
  "expected dictionary, got `string`",
)

// transformation : les champs sont reordonnes dans l'ordre du spec
#assert.eq(
  adt.validate(PAIR, (right: 2, left: 1)).value.keys(),
  ("left", "right"),
)

// enum : constructeurs a champs et nullaires
#let OPTION = adt.enum(__name__: "OPTION", nothing: none, some: int)
#let (intro: (nothing: opt-nothing, some: opt-some)) = adt.generate(OPTION)
#assert.eq(adt.validate(OPTION, opt-nothing), ok((__tag__: "nothing")))
#assert.eq(
  adt.validate(OPTION, opt-some(4)),
  ok((__tag__: "some", value: 4)),
)
#assert(result-is-err(adt.validate(OPTION, (__tag__: "unknown"))))
#assert(result-is-err(adt.validate(OPTION, "not-an-enum")))
#assert.eq(adt.validate(OPTION, "not-an-enum").msg, "not an enum value: \"not-an-enum\"")

// union : premiere branche qui matche gagne, valeur transformee de la branche
#let AB = adt.union(
  adt.struct(__name__: "A", a: int, b: int),
  adt.struct(__name__: "B", a: str, b: str),
)
#assert.eq(adt.validate(AB, (a: 1, b: 2)), ok((a: 1, b: 2)))
#assert.eq(adt.validate(AB, (a: "x", b: "y")), ok((a: "x", b: "y")))
// rollback : la branche A valide son champ `a` avec succes avant d'echouer
// sur `b` ; la pile de resultats doit etre nettoyee avant d'essayer B, et
// l'echec final doit porter l'erreur de la PREMIERE branche.
#let ab-mixed = adt.validate(AB, (a: 1, b: "y"))
#assert(result-is-err(ab-mixed))
#assert.eq(
  ab-mixed.msg,
  "expected a value of type `integer`, got `string`",
)
// union vide
#assert(result-is-err(adt.validate(adt.empty, none)))
#assert.eq(adt.validate(adt.empty, none).msg, "empty type has no values")

// fix : liste recursive classique
#let LIST = adt.fix(
  __name__: "list",
  self => adt.enum(
    __name__: "list",
    nil: none,
    cons: (head: int, tail: self),
  ),
)
#let (intro: (nil: list-nil, cons: list-cons)) = adt.generate(LIST)
#let one-two = list-cons(1, list-cons(2, list-nil))
#assert.eq(adt.validate(LIST, one-two), ok((
  __tag__: "cons",
  head: 1,
  tail: (__tag__: "cons", head: 2, tail: (__tag__: "nil")),
)))

// ---------------------------------------------------------------------
// 2. Fidelite des traces (memes chaines que l'ancienne implementation)
// ---------------------------------------------------------------------

#let array-error = adt.validate(adt.array(int), (1, "bad"))
#assert.eq(array-error.msg, "expected a value of type `integer`, got `string`")
#assert.eq(array-error.trace.__tag__, "trace/val")
#assert.eq(array-error.trace.cont.__tag__, "trace/array-val")
#assert.eq(array-error.trace.cont.index, 1)
#assert.eq(array-error.trace.cont.cont.__tag__, "trace/val")
#assert.eq(array-error.trace.cont.cont.cont.__tag__, "trace/root")
#assert.eq(adt.validate(adt.TRACE, array-error.trace), ok(array-error.trace))

#let dict-error = adt.validate(adt.dict(int), (good: 1, bad: "bad"))
#assert.eq(dict-error.trace.cont.__tag__, "trace/dictionary-val")
#assert.eq(dict-error.trace.cont.key, "bad")
#assert.eq(adt.validate(adt.TRACE, dict-error.trace), ok(dict-error.trace))

#let union-error = adt.validate(adt.union(int, str), none)
#assert.eq(union-error.trace.__tag__, "trace/val")
#assert.eq(union-error.trace.cont.__tag__, "trace/union")
#assert.eq(adt.validate(adt.TRACE, union-error.trace), ok(union-error.trace))

// chaine complete : struct -> champ -> union -> branche -> builtin
#let NESTED = adt.struct(__name__: "NESTED", x: adt.union(int, str))
#let nested-error = adt.validate(NESTED, (x: 1.5))
#assert(result-is-err(nested-error))
#assert.eq(nested-error.trace.__tag__, "trace/val")
#assert.eq(nested-error.trace.cont.__tag__, "trace/constr-field")
#assert.eq(nested-error.trace.cont.constr-arg, "x")
#assert.eq(nested-error.trace.cont.cont.__tag__, "trace/val")
#assert.eq(nested-error.trace.cont.cont.cont.__tag__, "trace/union")
#assert.eq(adt.validate(adt.TRACE, nested-error.trace), ok(nested-error.trace))

// enum : le nom de constructeur prefixe apparait dans trace/constr
#let named-enum-error = adt.validate(OPTION, (__tag__: "some", value: "bad"))
#assert.eq(named-enum-error.trace.__tag__, "trace/val")
#assert.eq(named-enum-error.trace.cont.__tag__, "trace/constr")
#assert.eq(named-enum-error.trace.cont.name, "OPTION-some")
#assert.eq(named-enum-error.trace.cont.cont.__tag__, "trace/constr-field")
#assert.eq(
  adt.validate(adt.TRACE, named-enum-error.trace),
  ok(named-enum-error.trace),
)

// struct : erreurs de champs manquants / en trop, payloads exacts
#assert.eq(
  adt.validate(PAIR, (left: 1)).trace.cont.__tag__,
  "trace/constr-missing-arg",
)
#assert.eq(
  adt.validate(PAIR, (left: 1, right: 2, z: 3)).trace.cont.__tag__,
  "trace/constr-extra-args",
)

// ---------------------------------------------------------------------
// 3. LA regression du ticket #7 : valeurs profondes, validation complete
// ---------------------------------------------------------------------

// construction ITERATIVE (aucune recursion cote test) d'une liste de 200
// elements : la validation complete descendait 200 niveaux de recursion
// avant, et explosait vers ~60.
#let deep-list = {
  let l = list-nil
  for i in range(200) {
    l = list-cons(i, l)
  }
  l
}
#let deep-list-result = adt.validate(LIST, deep-list)
#assert(result-is-ok(deep-list-result))
#assert.eq(deep-list-result.value, deep-list)

// arbre binaire profond (400 nœuds sur une branche gauche de 200 niveaux)
#let LEAF = adt.struct(__name__: "Leaf", name: str)
#let (intro: leaf) = adt.generate(LEAF)
#let TREE = adt.fix(
  __name__: "Tree",
  self => adt.enum(
    __name__: "Tree",
    leaf: LEAF,
    node: (left: self, right: self),
  ),
)
#let (intro: (leaf: tree-leaf, node: tree-node)) = adt.generate(TREE)
#let deep-tree = {
  let t = tree-leaf(leaf("bottom"))
  for i in range(200) {
    t = tree-node(t, tree-leaf(leaf(str(i))))
  }
  t
}
#let deep-tree-result = adt.validate(TREE, deep-tree)
#assert(result-is-ok(deep-tree-result))
#assert.eq(deep-tree-result.value, deep-tree)

// erreur ENFOUIE au fond d'une valeur profonde : doit produire une erreur
// propre (pas un crash), avec le bon message. La valeur invalide est
// construite en dict brut puis enterree sous 100 niveaux de nodes (les
// constructeurs generes ne valident les champs recursifs que superficiellement
// depuis le ticket #6, donc la construction passe).
#let buried-bad = {
  let t = (__tag__: "leaf", name: 42)
  for i in range(100) {
    t = (__tag__: "node", left: t, right: tree-leaf(leaf(str(i))))
  }
  t
}
#let buried-result = adt.validate(TREE, buried-bad)
#assert(result-is-err(buried-result))
#assert.eq(
  buried-result.msg,
  "expected a value of type `string`, got `integer`",
)
// et sa trace (profonde) est elle-meme un TRACE valide — nouvelle
// validation profonde par la machine.
#assert.eq(
  adt.validate(adt.TRACE, buried-result.trace),
  ok(buried-result.trace),
)

// profondeur NON recursive : structs imbriques a la main sur 150 niveaux
#let deep-spec = {
  let s = adt.struct(__name__: "L0", v: int)
  for i in range(150) {
    s = adt.struct(__name__: "L" + str(i + 1), inner: s)
  }
  s
}
#let deep-value = {
  let v = (v: 7)
  for i in range(150) {
    v = (inner: v)
  }
  v
}
#let deep-struct-result = adt.validate(deep-spec, deep-value)
#assert(result-is-ok(deep-struct-result))
#assert.eq(deep-struct-result.value, deep-value)

// union traversee en profondeur : chaque niveau propose deux branches dont
// la premiere echoue, forcant un backtracking a chaque etage.
#let deep-union-spec = {
  let s = adt.union(int, str)
  for i in range(80) {
    s = adt.union(adt.struct(__name__: "W" + str(i), w: int), adt.struct(__name__: "U" + str(i), u: s))
  }
  s
}
#let deep-union-value = {
  let v = "bottom"
  for i in range(80) {
    v = (u: v)
  }
  v
}
#let deep-union-result = adt.validate(deep-union-spec, deep-union-value)
#assert(result-is-ok(deep-union-result))
#assert.eq(deep-union-result.value, deep-union-value)
