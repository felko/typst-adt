/// Spec for result values.
///
/// A `RESULT(T)` is either `ok(value)` where `value` matches `T`, or `err(msg)`
/// where `msg` is a string.
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

/// Creates a successful result.
///
/// - `x`: Value to store.
#let ok(x) = (
  __tag__: "result/ok",
  value: x,
)

/// Creates a failed result.
///
/// - `e`: Error message. Converted to a string.
#let err(e) = (
  __tag__: "result/err",
  msg: str(e),
)

/// Pattern matches a result.
///
/// Pass exactly two named cases: `ok:` and `err:`. Non-function cases are
/// treated as constants.
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

/// Maps an ok result.
///
/// - `f`: Function applied to the ok value.
/// - `result`: Result to map.
#let result-map(f, result) = result-elim(
  ok: value => ok(f(value)),
  err: err,
)(result)

/// Maps two ok results.
///
/// Calls `f(value1, value2)` only when both inputs are ok. The first error is
/// returned unchanged.
#let result-map2(f, result1, result2) = result-elim(
  ok: value1 => result-elim(
    ok: value2 => ok(f(value1, value2)),
    err: err,
  )(result2),
  err: err,
)(result1)

/// Returns `result1` if it is ok, otherwise returns `result2`.
#let result-or-else(result1, result2) = result-elim(
  ok: ok,
  err: result2,
)(result1)

/// Chains a result into a result-producing continuation.
///
/// Calls `cont(value)` for ok results and passes errors through unchanged.
#let result-and-then(result, cont) = {
  result-elim(ok: cont, err: err)(result)
}

/// Returns whether `result` is an ok result.
#let result-is-ok(result) = result.__tag__ == "result/ok"

/// Returns whether `result` is an err result.
#let result-is-err(result) = result.__tag__ == "result/err"

/// Maps an array with a result-producing function.
///
/// Returns `ok(values)` when all items are ok. Returns one error containing all
/// indexed failures otherwise.
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

/// Maps dictionary values with a result-producing function.
///
/// Keys are preserved. Returns the first aggregated error from `result-all`.
#let result-all-dict(f, xs) = result-map(
  pairs => pairs.to-dict(),
  result-all(
    ((k, v)) => result-map(w => (k, w), f(v)),
    xs.pairs(),
  ),
)

/// Zips two arrays with a result-producing function.
///
/// Returns an error on arity mismatch.
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

/// Zips matching dictionary keys with a result-producing function.
///
/// Returns an error when either dictionary is missing keys from the other.
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

/// Returns the first ok result from mapping an array.
///
/// If every item fails, returns one error containing all failures.
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

/// Extracts the ok value.
///
/// Panics with the error message when given an err result.
#let result-unwrap(result) = {
  if result.__tag__ == "result/ok" {
    result.value
  } else if result.__tag__ == "result/err" {
    panic(result.msg)
  } else {
    panic("invalid result: `" + repr(result) + "`")
  }
}

/// Validates an ok result value.
///
/// Err results pass through unchanged.
#let result-validate(result, validate) = {
  if result.__tag__ == "result/ok" {
    ok(validate(result.value))
  } else if result.__tag__ == "result/err" {
    result
  } else {
    panic("invalid result: `" + repr(result) + "`")
  }
}
