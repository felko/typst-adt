// Spec for result values: either `ok(value)` or `err(msg)`.
#let RESULT(T) = (
  __tag__: "spec/enum",
  name: auto,
  constrs: (
    ok: (
      __tag__: "constr-spec/fields",
      fields: (
        value: T,
      ),
    ),
    err: (
      __tag__: "constr-spec/fields",
      fields: (
        msg: (__tag__: "spec/builtin", value: str),
      ),
    ),
  ),
)

// Wraps a successful result value.
#let ok(x) = (
  __tag__: "result/ok",
  value: x,
)

// Wraps a failed result message.
#let err(e) = (
  __tag__: "result/err",
  msg: str(e),
)

// Pattern matches a result with `ok:` and `err:` cases.
#let result-elim(..args) = {
  assert(
    args.pos().len() == 0,
    message: "expected no positional arguments",
  )
  let cases = args.named()
  if not cases.keys().contains("ok") and not cases.keys().contains("err") {
    panic("missing cases: `ok`, `err`")
  } else if not cases.keys().contains("ok") {
    panic("missing case: `ok`")
  } else if not cases.keys().contains("err") {
    panic("missing case: `err`")
  }
  let (ok: ok-case, err: err-case, ..cases) = cases
  if type(ok-case) != function {
    ok-case = _ => ok-case
  }
  if cases.len() > 0 {
    panic(
      "too many cases: "
        + cases.pairs().map(((k, v)) => "`" + k + "`").join(", "),
    )
  }
  if type(err-case) != function {
    err-case = _ => err-case
  }
  result => {
    if result.__tag__ == "result/ok" {
      ok-case(result.value)
    } else if result.__tag__ == "result/err" {
      err-case(result.msg)
    } else {
      panic("invalid result: `" + repr(result) + "`")
    }
  }
}

// Applies `f` to the value inside an ok result.
#let result-map(f, result) = result-elim(
  ok: value => ok(f(value)),
  err: err,
)(result)

// Applies `f` when both results are ok.
#let result-map2(f, result1, result2) = result-elim(
  ok: value1 => result-elim(
    ok: value2 => ok(f(value1, value2)),
    err: err,
  )(result2),
  err: err,
)(result1)

// Returns the first result if ok, otherwise returns the second.
#let result-or-else(result1, result2) = result-elim(
  ok: ok,
  err: result2,
)(result1)

// Chains a result into a function that returns another result.
#let result-and-then(result, cont) = {
  result-elim(ok: cont, err: err)(result)
}

// Tests whether a result is ok.
#let result-is-ok(result) = result.__tag__ == "result/ok"

// Tests whether a result is err.
#let result-is-err(result) = result.__tag__ == "result/err"

// Runs `f` over an array and collects all ok values.
#let result-all(f, xs) = {
  let ys = ()
  let errs = (:)
  for (i, x) in xs.enumerate() {
    let y = f(x)
    if y.__tag__ == "result/ok" {
      ys.push(y.value)
    } else if y.__tag__ == "result/err" {
      errs.insert(str(i), y.msg)
    } else {
      panic("invalid result: `" + repr(y) + "`")
    }
  }
  if errs.len() == 0 {
    ok(ys)
  } else {
    err(errs.pairs().map(((i, msg)) => "at index " + i + ": " + msg).join("\n"))
  }
}

// Runs `f` over dictionary values and rebuilds the dictionary.
#let result-all-dict(f, xs) = result-map(
  pairs => pairs.to-dict(),
  result-all(
    ((k, v)) => result-map(w => (k, w), f(v)),
    xs.pairs(),
  ),
)

// Zips two arrays with a result-returning function.
#let result-zip(f, xs, ys) = {
  if xs.len() == ys.len() {
    err("arity mismatch: `" + repr(xs) + "` vs. `" + repr(ys) + "`")
  } else {
    result-all(
      ((x, y)) => f(x, y),
      xs.zip(ys),
    )
  }
}

// Zips matching dictionary keys with a result-returning function.
#let result-zip-dict(f, xs, ys) = {
  assert.eq(type(xs), dictionary)
  assert.eq(type(ys), dictionary)
  let (missing-left, missing-right) = ((), ())
  let zipped = (:)
  for (kx, vx) in xs.pairs() {
    if ys.keys().contains(kx) {
      let vy = ys.remove(kx)
      zipped.insert(kx, (vx, vy))
    } else {
      missing-right.push(kx)
    }
  }
  for ky in ys.keys() {
    missing-left.push(ky)
  }

  if missing-left.len() > 0 and missing-right.len() > 0 {
    err(
      "missing "
        + missing-left.map(k => "`" + k + "`").join(", ")
        + " from the left and "
        + missing-right.map(k => "`" + k + "`").join(", ")
        + " from the right",
    )
  } else if missing-left.len() > 0 {
    err(
      "missing "
        + missing-left.map(k => "`" + k + "`").join(", ")
        + " from the left",
    )
  } else if missing-right.len() > 0 {
    err(
      "missing "
        + missing-right.map(k => "`" + k + "`").join(", ")
        + " from the right",
    )
  } else {
    result-all-dict(
      ((vx, vy)) => f(vx, vy),
      zipped,
    )
  }
}

// Returns the first ok result from trying `f` over an array.
#let result-any(f, xs) = {
  let errs = ()
  for (i, x) in xs.enumerate() {
    let y = f(x)
    if y.__tag__ == "result/ok" {
      return y
    } else if y.__tag__ == "result/err" {
      errs.push("at index " + str(i) + ": " + y.msg)
    } else {
      panic("invalid result: `" + repr(y) + "`")
    }
  }
  err("found no result:\n" + errs.join("\n"))
}

// Extracts an ok value or panics with the error message.
#let result-unwrap(result) = {
  if result.__tag__ == "result/ok" {
    result.value
  } else if result.__tag__ == "result/err" {
    panic(result.msg)
  } else {
    panic("invalid result: `" + repr(result) + "`")
  }
}

// Validates an ok result value, passing errors through unchanged.
#let result-validate(result, validate) = {
  if result.__tag__ == "result/ok" {
    ok(validate(result.value))
  } else if result.__tag__ == "result/err" {
    result
  } else {
    panic("invalid result: `" + repr(result) + "`")
  }
}
