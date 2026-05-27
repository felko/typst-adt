#import "../../src/lib.typ" as adt: ok

#let OPTION(T) = adt.enum(
  __name__: "OPTION(" + adt.to-string(T) + ")",
  nothing: none,
  some: T,
)

#let (
  intro: (
    nothing: option-nothing,
    some: option-some,
  ),
  elim: option-elim,
) = adt.generate(OPTION(int))

#assert.eq(option-nothing.__tag__, "nothing")
#assert.eq(option-some(4).value, 4)
#assert.eq(option-elim(nothing: 0, some: value => value)(option-nothing), 0)
#assert.eq(option-elim(nothing: 0, some: value => value)(option-some(5)), 5)

#let LIST(T) = adt.fix(
  __name__: "list(" + adt.to-string(T) + ")",
  self => adt.enum(
    __name__: "list.base("
      + adt.to-string(T)
      + ", "
      + adt.to-string(self)
      + ")",
    nil: none,
    cons: (head: T, tail: self),
  ),
)

#let (
  intro: (
    nil: list-nil,
    cons: list-cons,
  ),
  fields: (
    head: list-head,
    tail: list-tail,
  ),
  elim: list-elim,
  rec: list-rec,
) = adt.generate(LIST(int))

#let one-two = list-cons(1, list-cons(2, list-nil))
#assert.eq(list-nil.__tag__, "nil")
#assert.eq(list-cons(head: 1, tail: list-nil).head, 1)
#assert.eq(adt.validate(LIST(int), one-two), ok((
  __tag__: "cons",
  head: 1,
  tail: (__tag__: "cons", head: 2, tail: (__tag__: "nil")),
)))
#assert.eq(list-head(one-two), 1)
#assert.eq(list-tail(one-two).head, 2)

#let list-len = list-rec(
  nil: 0,
  cons: (head, tail-len) => tail-len + 1,
)
#assert.eq(list-len(list-nil), 0)
#assert.eq(list-len(one-two), 2)

#let list-append(l1, l2) = list-rec(
  nil: l2,
  cons: list-cons,
)(l1)

#let list(..args) = {
  let xs = list-nil
  for elem in args.pos().rev() {
    xs = list-cons(elem, xs)
  }
  xs
}

#assert.eq(list-append(list(1, 2), list(3)), list(1, 2, 3))

#let (annotate: list-annotate) = adt.generate(LIST(int))
#let SIZED-LIST = adt.annotate(LIST(int), size: int)
#let (elim: sized-list-elim) = adt.generate(SIZED-LIST)

#let sized = list-annotate(
  __ann__: (size: int),
  nil: (size: 0),
  cons: (head, tail) => (size: tail.size + 1),
)(list(1, 2))

#assert.eq(sized.size, 2)
#assert.eq(sized.tail.size, 1)
#assert.eq(adt.validate(SIZED-LIST, sized), ok(sized))
#assert.eq(
  sized-list-elim(
    nil: 0,
    cons: (head, tail, size) => head + tail.size,
  )(sized),
  2,
)

#let SIZED-SUMMED-LIST = adt.annotate(LIST(int), size: int, sum: int)

#let sized-and-summed = list-annotate(
  __ann__: (size: int, sum: int),
  nil: (size: 0, sum: 0),
  cons: (head, (size: tail-size, sum: tail-sum)) => (
    size: tail-size + 1,
    sum: tail-sum + head,
  ),
)(list(1, 2))

#assert.eq(sized-and-summed.size, 2)
#assert.eq(sized-and-summed.sum, 3)
#assert.eq(sized-and-summed.tail.size, 1)
#assert.eq(sized-and-summed.tail.sum, 2)
#assert.eq(adt.validate(SIZED-SUMMED-LIST, sized-and-summed), ok(sized-and-summed))

#let max2(x, y) = if x > y { x } else { y }

#let TREE(T) = adt.fix(
  __name__: "tree(" + adt.to-string(T) + ")",
  self => adt.enum(
    leaf: T,
    node: (left: self, right: self),
  ),
)

#let (
  intro: (
    leaf: tree-leaf,
    node: tree-node,
  ),
  rec: tree-rec,
  elim: tree-elim,
  annotate: tree-annotate,
) = adt.generate(TREE(str))

#let tree = tree-node(
  tree-node(tree-leaf("a"), tree-leaf("b")),
  tree-leaf("c"),
)

#let HEIGHTED-TREE = adt.annotate(TREE(str), height: int)

#let heighted = tree-annotate(
  __ann__: (height: int),
  leaf: value => (height: 0),
  node: (left, right) => {
    (height: max2(left.height, right.height) + 1)
  },
)(tree)

#assert.eq(heighted.height, 2)
#assert.eq(heighted.left.height, 1)
#assert.eq(heighted.left.left.height, 0)
#assert.eq(heighted.right.height, 0)
#assert.eq(adt.validate(HEIGHTED-TREE, heighted), ok(heighted))

#let HEIGHTED-DEPTHED-TREE = adt.annotate(TREE(str), height: int, depth: int)
#let (
  intro: (
    leaf: heighted-depthed-leaf,
    node: heighted-depthed-node,
  ),
) = adt.generate(HEIGHTED-DEPTHED-TREE)
#let (rec: heighted-tree-rec) = adt.generate(HEIGHTED-TREE)

#let annotate-depth(tree) = tree-rec(
  leaf: value => depth => tree-leaf(
    value,
    __ann__: (depth: depth,),
  ),
  node: (left, right) => depth => tree-node(
    left(depth + 1),
    right(depth + 1),
    __ann__: (
      depth: depth,
    )
  ),
)(tree)(0)

#let annotate-depth(tree) = adt.rec(
  adt.annotate(
    TREE(str),
    height: int,
    depth: int
  ),
  leaf: (value, height) => depth => tree-leaf(
    value,
    __ann__: (
      height: height,
      depth: depth,
    ),
  ),
  node: (left, right, height) => depth => tree-node(
    left(depth + 1),
    right(depth + 1),
    __ann__: (
      height: height,
      depth: depth,
    )
  ),
)(tree)(0)

#let heighted-and-depthed = annotate-depth(heighted)

#assert.eq(heighted-and-depthed.height, 2)
#assert.eq(heighted-and-depthed.depth, 0)
#assert.eq(heighted-and-depthed.left.height, 1)
#assert.eq(heighted-and-depthed.left.depth, 1)
#assert.eq(heighted-and-depthed.left.left.height, 0)
#assert.eq(heighted-and-depthed.left.left.depth, 2)
#assert.eq(heighted-and-depthed.right.height, 0)
#assert.eq(heighted-and-depthed.right.depth, 1)
#assert.eq(
  validate(HEIGHTED-DEPTHED-TREE, heighted-and-depthed),
  ok(heighted-and-depthed),
)
#assert.eq(
  tree-elim(
    leaf: value => value,
    node: (left, right) => left.left.value,
  )(heighted-and-depthed),
  "a",
)
