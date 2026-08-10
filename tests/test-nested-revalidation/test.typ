#import "../../src/lib.typ" as adt: ok, result-is-err

// Reproduit un "maximum function call depth exceeded" observe dans WorldTradeFinance4 :
// une chaine struct -> enum -> struct -> union suffisamment profonde, une fois
// revalidee comme champ d'un struct englobant supplementaire, depasse la limite
// d'appels Typst alors que la construction "a plat" des memes valeurs ne pose
// aucun probleme.

#let LEAF_A = adt.struct(__name__: "LeafA", name: str)
#let LEAF_B = adt.struct(__name__: "LeafB", name: str, members: adt.array(str))
#let OWNERISH = adt.union(LEAF_A, LEAF_B)

#let (intro: leaf-a) = adt.generate(LEAF_A)
#let (intro: leaf-b) = adt.generate(LEAF_B)

#let MID = adt.struct(__name__: "Mid", owner: OWNERISH)
#let (intro: mid) = adt.generate(MID)

#let OUTER = adt.enum(
  __name__: "Outer",
  simple: (name: str,),
  wrapped: MID,
)
#let (intro: (simple: outer-simple, wrapped: outer-wrapped)) = adt.generate(OUTER)

#let ACCOUNTISH = adt.struct(__name__: "Accountish", actor: OUTER)
#let (intro: accountish) = adt.generate(ACCOUNTISH)

#let LEGISH = adt.struct(__name__: "Legish", account: ACCOUNTISH, amount: float)
#let (intro: legish, fields: (account: legish-account)) = adt.generate(LEGISH)

#let TXNISH = adt.struct(__name__: "Txnish", legs: adt.array(LEGISH))
#let (intro: txnish, fields: (legs: txnish-legs)) = adt.generate(TXNISH)

// Construction directe avec un acteur "profond" (wrapped -> Mid -> owner union) :
// ne pose aucun probleme, c'est la premiere validation.
#let deep-actor = outer-wrapped(mid(leaf-b("group", ("a", "b"))))
#let leg = legish(accountish(deep-actor), 100.0)
#let txn = txnish((leg,))
#assert.eq(adt.validate(TXNISH, txn), ok(txn))

// Enveloppe cette valeur DEJA construite/taguee dans un struct englobant
// supplementaire : c'est ce pattern precis (checked-txn deja construit passe
// en argument d'un Commit) qui declenchait le depassement de profondeur.
#let CHECKEDISH = adt.struct(
  __name__: "Checkedish",
  txn: TXNISH,
  balances: adt.dict(adt.dict(float)),
  balanced: bool,
)
#let (intro: checkedish) = adt.generate(CHECKEDISH)

#let checked = checkedish(txn, (alice: (usd: 10.0)), true)
#assert.eq(adt.validate(CHECKEDISH, checked), ok(checked))

// Un niveau d'enveloppe de plus : struct englobant CHECKEDISH lui-meme.
// C'est ce niveau qui reproduit le "maximum function call depth exceeded".
#let COMMITISH = adt.struct(__name__: "Commitish", txns: adt.array(CHECKEDISH))
#let (intro: commitish) = adt.generate(COMMITISH)

#let commit = commitish((checked,))
#assert.eq(adt.validate(COMMITISH, commit), ok(commit))
