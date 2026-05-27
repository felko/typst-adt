/// Spec for result values.
///
/// A `RESULT(T)` is either `ok(value)` where `value` matches `T`, or `err(msg)`
/// where `msg` is a string.
/// -> spec
#let RESULT(
  /// Spec for the successful value.
  /// -> spec
  T,
) = (
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
/// -> RESULT(any)
#let ok(
  /// Value to store.
  /// -> any
  x,
) = (
  __tag__: "result/ok",
  value: x,
)

/// Creates a failed result.
///
/// - `e`: Error message. Converted to a string.
/// -> RESULT(any)
#let err(
  /// Error message.
  /// -> any
  e,
) = (
  __tag__: "result/err",
  msg: str(e),
)

/// Pattern matches a result.
///
/// Pass exactly two named cases: `ok:` and `err:`. Non-function cases are
/// treated as constants.
/// -> function
#let result-elim(
  /// Named `ok` and `err` cases.
  /// -> arguments
  ..args,
) = {
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
/// -> RESULT(any)
#let result-map(
  /// Function applied to the ok value.
  /// -> function
  f,
  /// Result to map.
  /// -> RESULT(any)
  result,
) = result-elim(
  ok: value => ok(f(value)),
  err: err,
)(result)

/// Maps two ok results.
///
/// Calls `f(value1, value2)` only when both inputs are ok. The first error is
/// returned unchanged.
/// -> RESULT(any)
#let result-map2(
  /// Function applied to both ok values.
  /// -> function
  f,
  /// First result.
  /// -> RESULT(any)
  result1,
  /// Second result.
  /// -> RESULT(any)
  result2,
) = result-elim(
  ok: value1 => result-elim(
    ok: value2 => ok(f(value1, value2)),
    err: err,
  )(result2),
  err: err,
)(result1)

/// Returns `result1` if it is ok, otherwise returns `result2`.
/// -> RESULT(any)
#let result-or-else(
  /// Preferred result.
  /// -> RESULT(any)
  result1,
  /// Fallback result.
  /// -> RESULT(any)
  result2,
) = result-elim(
  ok: ok,
  err: result2,
)(result1)

/// Chains a result into a result-producing continuation.
///
/// Calls `cont(value)` for ok results and passes errors through unchanged.
/// -> RESULT(any)
#let result-and-then(
  /// Input result.
  /// -> RESULT(any)
  result,
  /// Continuation returning a result.
  /// -> function
  cont,
) = {
  result-elim(ok: cont, err: err)(result)
}

/// Returns whether `result` is an ok result.
#let result-is-ok(
  /// Result to test.
  /// -> RESULT(any)
  result,
) = result.__tag__ == "result/ok"

/// Returns whether `result` is an err result.
#let result-is-err(
  /// Result to test.
  /// -> RESULT(any)
  result,
) = result.__tag__ == "result/err"

/// Maps an array with a result-producing function.
///
/// Returns `ok(values)` when all items are ok. Returns one error containing all
/// indexed failures otherwise.
/// -> RESULT(array)
#let result-all(
  /// Function returning a result for each item.
  /// -> function
  f,
  /// Items to map.
  /// -> array
  xs,
) = {
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
/// -> RESULT(dictionary)
#let result-all-dict(
  /// Function returning a result for each value.
  /// -> function
  f,
  /// Dictionary to map.
  /// -> dictionary
  xs,
) = result-map(
  pairs => pairs.to-dict(),
  result-all(
    ((k, v)) => result-map(w => (k, w), f(v)),
    xs.pairs(),
  ),
)

/// Zips two arrays with a result-producing function.
///
/// Returns an error on arity mismatch.
/// -> RESULT(array)
#let result-zip(
  /// Function called with paired values.
  /// -> function
  f,
  /// First array.
  /// -> array
  xs,
  /// Second array.
  /// -> array
  ys,
) = {
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
/// -> RESULT(dictionary)
#let result-zip-dict(
  /// Function called with paired values.
  /// -> function
  f,
  /// First dictionary.
  /// -> dictionary
  xs,
  /// Second dictionary.
  /// -> dictionary
  ys,
) = {
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
/// -> RESULT(any)
#let result-any(
  /// Function returning a result for each item.
  /// -> function
  f,
  /// Items to try.
  /// -> array
  xs,
) = {
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
/// -> any
#let result-unwrap(
  /// Result to unwrap.
  /// -> RESULT(any)
  result,
) = {
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
/// -> RESULT(any)
#let result-validate(
  /// Result to validate.
  /// -> RESULT(any)
  result,
  /// Validator for the ok value.
  /// -> function
  validate,
) = {
  if result.__tag__ == "result/ok" {
    ok(validate(result.value))
  } else if result.__tag__ == "result/err" {
    result
  } else {
    panic("invalid result: `" + repr(result) + "`")
  }
}
