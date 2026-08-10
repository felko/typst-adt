// Test de non-regression : specs profondement imbriques avec sous-types
// partages entre branches (forme du projet WorldTradeFinance4).
//
// Avant le fix, `spec-contains-self` / `spec-shallow` (methods/common.typ)
// etaient recursives natives et re-traversaient tout l'arbre du spec a chaque
// champ de chaque construction (via project-constr-args / project-constr).
// Chaque niveau logique de spec coutait ~4-6 cadres de pile Typst
// (spec-contains-self -> spec-elim -> lambda -> .any() -> closure), si bien
// qu'un spec de ~13+ niveaux suffisait a depasser la limite native
// "maximum function call depth exceeded" -- sans aucune valeur circulaire.
//
// Ce test construit une hierarchie de ~19 niveaux logiques (Commit -> array
// -> Checked -> Txn -> array -> Leg -> Account -> Value -> Equity -> Issuer
// -> Portfolio -> attributes -> Owner -> Group -> array -> User -> Currency
// -> attributes -> CurrencyType) et doit compiler sans erreur.

#import "../../src/lib.typ" as adt: ok, result-is-ok

// ---------------------------------------------------------------------
// 1. Les specs (memes formes que WTF4, sous-types partages entre branches)
// ---------------------------------------------------------------------

#let CURRENCY_TYPE = adt.enum(
  __name__: "CurrencyType",
  national: none,
  crypto: none,
  legacy: none,
  bullion: none,
)
#let CURRENCY = adt.struct(
  __name__: "Currency",
  name: str,
  attributes: (type: CURRENCY_TYPE),
)
#let USER = adt.struct(__name__: "User", name: str, reportcurrency: CURRENCY)
#let GROUP = adt.struct(
  __name__: "Group",
  name: str,
  owner: USER,
  members: adt.array(USER),
)
#let OWNER = adt.union(USER, GROUP)
#let PORTFOLIO = adt.struct(
  __name__: "Portfolio",
  name: str,
  attributes: (owner: OWNER, reportcurrency: CURRENCY, initialsharevalue: float),
)
#let COMPANY = adt.struct(
  __name__: "Company",
  name: str,
  attributes: (industry: str, sector: str, reportcurrency: CURRENCY),
)
// PORTFOLIO reutilise depuis plusieurs branches : ISSUER, DEBTABLE, ACTOR.
#let ISSUER = adt.enum(__name__: "Issuer", portfolio: PORTFOLIO, company: COMPANY)
#let FIBO = adt.enum(
  __name__: "Fibo",
  common-share: none,
  common-share-in-fund: none,
)
#let EQUITY = adt.struct(
  __name__: "Equity",
  name: str,
  issuer: ISSUER,
  fibo-type: FIBO,
)
#let FREQUENCY = adt.struct(__name__: "Frequency", name: str, annual: int)
#let DEBTABLE = adt.union(PORTFOLIO, COMPANY, USER)
#let DEBT = adt.struct(
  __name__: "Debt",
  name: str,
  issuer: DEBTABLE,
  coupon: float,
  frequency: FREQUENCY,
)
#let VALUE = adt.enum(__name__: "Value", equity: EQUITY, currency: CURRENCY, debt: DEBT)
#let BANK = adt.struct(__name__: "Bank", devise: str)
#let ACTOR = adt.enum(
  __name__: "Actor",
  user: USER,
  portfolio: PORTFOLIO,
  bank: BANK,
)
#let ROLE = adt.enum(__name__: "Role", long: none, short: none, reference: none)
#let NATURE = adt.enum(__name__: "Nature", actif: none, passif: none)
#let ACCOUNT = adt.enum(
  __name__: "Account",
  cash: (actor: ACTOR, currency: CURRENCY),
  cash-bank: (bank: BANK, client: ACTOR, currency: CURRENCY),
  pcg: (nature: NATURE, actor: ACTOR, pcg-id: int),
  // Le variant critique : deux champs VALUE cote a cote.
  exposure: (actor: ACTOR, base: VALUE, quote: VALUE, role: ROLE),
  portfolio: (actor: ACTOR, portfolio: PORTFOLIO, fibo-type: FIBO),
  share-portfolio: (portfolio: PORTFOLIO, fibo-type: FIBO),
  shareholder: (shareholder: ACTOR, portfolio: PORTFOLIO, fibo-type: FIBO),
)
#let LEG = adt.struct(__name__: "Leg", account: ACCOUNT, amount: float)
#let TXN = adt.struct(__name__: "Txn", name: str, legs: adt.array(LEG))
#let CHECKED = adt.struct(__name__: "Checked", txn: TXN, balanced: bool)
#let PARENT(T) = adt.enum(__name__: "Parent", root: none, ref: T)
#let COMMIT = adt.fix(
  __name__: "Commit",
  self => adt.struct(
    __name__: "Commit",
    parent1: PARENT(self),
    txns: adt.array(CHECKED),
  ),
)

// ---------------------------------------------------------------------
// 2. Generation des constructeurs / eliminateurs
// ---------------------------------------------------------------------

#let (
  intro: (national: ctype-national,),
) = adt.generate(CURRENCY_TYPE)
#let (intro: Currency,) = adt.generate(CURRENCY)
#let (intro: User,) = adt.generate(USER)
#let (intro: Group,) = adt.generate(GROUP)
#let (intro: Portfolio,) = adt.generate(PORTFOLIO)
#let (intro: Company,) = adt.generate(COMPANY)
#let (intro: (portfolio: issuer-portfolio,),) = adt.generate(ISSUER)
#let (intro: (common-share: fibo-cs,),) = adt.generate(FIBO)
#let (intro: Equity,) = adt.generate(EQUITY)
#let (intro: Frequency,) = adt.generate(FREQUENCY)
#let (intro: Debt,) = adt.generate(DEBT)
#let (
  intro: (equity: value-equity, currency: value-currency,),
) = adt.generate(VALUE)
#let (intro: Bank,) = adt.generate(BANK)
#let (
  intro: (user: actor-user, bank: actor-bank,),
) = adt.generate(ACTOR)
#let (intro: (long: role-long,),) = adt.generate(ROLE)
#let (intro: (passif: nature-passif,),) = adt.generate(NATURE)
#let (
  intro: (
    cash: Account-cash,
    exposure: Account-exposure,
    shareholder: Account-shareholder,
  ),
  elim: Account-elim,
) = adt.generate(ACCOUNT)
#let (
  intro: Leg,
  fields: (account: Leg-account, amount: Leg-amount),
) = adt.generate(LEG)
#let (intro: Txn,) = adt.generate(TXN)
#let (intro: Checked,) = adt.generate(CHECKED)
#let (intro: Commit-raw,) = adt.generate(COMMIT)
#let (
  intro: (root: parent-root, ref: parent-ref),
) = adt.generate(PARENT(COMMIT))

// ---------------------------------------------------------------------
// 3. Valeurs de base
// ---------------------------------------------------------------------

#let usd = Currency("USD", (type: ctype-national))
#let alice = User("Alice", usd)
#let tech = Portfolio(
  "Tech",
  (owner: alice, reportcurrency: usd, initialsharevalue: 1.0),
)
#let acme = Company("ACME", (industry: "tech", sector: "soft", reportcurrency: usd))
#let eq = Equity("TECH-SHARE", issuer-portfolio(tech), fibo-cs)
#let debt = Debt("ACME-BOND", acme, 0.03, Frequency("annual", 1))
#let a-alice = actor-user(alice)
#let wtf-bank = Bank("USD")

#repr("valeurs de base OK")

// ---------------------------------------------------------------------
// 4. Constructions profondes -- avant le fix, ca explosait ici
//    ("maximum function call depth exceeded" des la construction d'un
//    compte exposure, puis pire encore sur Leg/Txn/Commit).
// ---------------------------------------------------------------------

#let acc-expo = Account-exposure(a-alice, value-equity(eq), value-currency(usd), role-long)
#let acc-cash = Account-cash(a-alice, usd)
#let acc-sh = Account-shareholder(a-alice, tech, fibo-cs)

#repr("comptes OK")

// Mimique de Sub-Txn/Txn-check de WTF4 : quelques cadres d'appels
// utilisateur en plus au-dessus des constructions.
#let make-legs(account, amount) = (
  Leg(account, amount),
  Leg(acc-cash, -amount),
)
#let txn-check(name, legs) = Checked(Txn(name, legs), true)

#let checked = txn-check("expo", make-legs(acc-expo, 100.0))

#repr("txn OK")

// Simule la profondeur de base du vrai projet (chaine d'imports imbriques
// + Ledger-push -> Txn-deposit -> Value-elim -> Sub-Txn -> intro...) :
// la limite native de Typst est ~64-80 cadres comptes, et la traversee
// recursive du spec (~18 niveaux logiques) consommait a elle seule ~50-60
// cadres -- c'est le cumul des deux qui depassait la limite dans WTF4.
#let nest(n, f) = if n <= 0 { f() } else { nest(n - 1, f) }

// Chaine de commits construite en boucle (comme Ledger-push), sous 30
// cadres utilisateur. Avant le fix : "maximum function call depth
// exceeded" ici. Apres le fix : la traversee du spec est O(1) en pile,
// donc large marge.
#let chain = nest(30, () => {
  let commit = Commit-raw(parent-root, ())
  for i in range(30) {
    commit = Commit-raw(parent-ref(commit), (checked,))
  }
  commit
})

#repr("chaine de commits OK")

// ---------------------------------------------------------------------
// 5. Eliminations (chemin project-constr) + assertions
// ---------------------------------------------------------------------

// Accesseur de champ sur un struct dont le spec est profond.
#assert.eq(Leg-amount(Leg(acc-expo, 42.0)), 42.0)

// Eliminateur d'enum sur le variant profond.
#let expo-base-name = Account-elim(
  cash: (..) => none,
  cash-bank: (..) => none,
  pcg: (..) => none,
  exposure: (actor, base, quote, role) => base.value.name,
  portfolio: (..) => none,
  share-portfolio: (..) => none,
  shareholder: (..) => none,
)(acc-expo)
#assert.eq(expo-base-name, "TECH-SHARE")

// La chaine de commits fait bien 30 maillons au-dessus de la racine.
#let chain-len = {
  let c = chain
  let n = 0
  while c.parent1.__tag__ == "ref" {
    n += 1
    c = c.parent1.value
  }
  n
}
#assert.eq(chain-len, 30)

// Boucle d'amplification : 20 constructions supplementaires de Leg profonds.
#for i in range(20) {
  let leg = Leg(Account-exposure(a-alice, value-equity(eq), value-currency(usd), role-long), 1.0)
  assert.eq(Leg-amount(leg), 1.0)
}

#repr("test-deep-spec: tout est passe")
