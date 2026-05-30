#import "std.typ" as std

/// Spec for result values.
///
/// A `RESULT(T)` is either `ok(value)` where `value` matches `T`, or an error
/// with a primary message and structured trace.
/// -> spec
#let RESULT-AUX(
  to-string,
  TRACE,
  /// Spec for the successful value.
  /// -> spec
  T,
) = (
  __tag__: "spec/enum",
  name: "RESULT(" + to-string(T) + ")",
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
        msg: (__tag__: "spec/builtin", name: "string", value: str),
        trace: TRACE,
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
/// - `msg`: Primary error message. Converted to a string.
/// -> RESULT(any)
#let err(
  /// Error message.
  /// -> any
  msg,
  /// Structured error trace.
  /// -> trace
  trace: (__tag__: "trace/root"),
) = (
  __tag__: "result/err",
  msg: str(msg),
  trace: trace,
)

/// Adds an outer frame to an error trace.
///
/// Ok results pass through unchanged.
/// -> RESULT(any)
#let result-trace(frame, result) = {
  if result.__tag__ == "result/ok" {
    result
  } else if result.__tag__ == "result/err" {
    (
      __tag__: "result/err",
      msg: result.msg,
      trace: frame(result.trace),
    )
  } else {
    panic("invalid result", result)
  }
}

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
  if std.type(ok-case) != std.function {
    ok-case = _ => ok-case
  }
  if cases.len() > 0 {
    panic(
      "too many cases: "
        + cases.pairs().map(((k, v)) => "`" + k + "`").join(", "),
    )
  }
  if std.type(err-case) != std.function {
    err-case = _ => err-case
  }
  result => {
    if std.type(result) != std.dictionary {
      panic("invalid result: `" + repr(result) + "`")
    }
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
  err: _ => result,
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
    err: _ => result2,
  )(result2),
  err: _ => result1,
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
  err: _ => result2,
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
  result-elim(ok: cont, err: _ => result)(result)
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
/// Returns `ok(values)` when all items are ok. Returns the first indexed error
/// otherwise.
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
  for (i, x) in xs.enumerate() {
    let y = f(x)
    if y.__tag__ == "result/ok" {
      ys.push(y.value)
    } else if y.__tag__ == "result/err" {
      return result-trace(
        cont => (
          __tag__: "trace/array-val",
          index: i,
          value: x,
          cont: cont,
        ),
        y,
      )
    } else {
      panic("invalid result", y)
    }
  }
  ok(ys)
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
) = {
  let ys = (:)
  for (key, value) in xs.pairs() {
    let result = f(value)
    if result.__tag__ == "result/ok" {
      ys.insert(key, result.value)
    } else if result.__tag__ == "result/err" {
      return result-trace(
        cont => (
          __tag__: "trace/dictionary-val",
          key: key,
          value: value,
          cont: cont,
        ),
        result,
      )
    } else {
      panic("invalid result", result)
    }
  }
  ok(ys)
}

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
  if xs.len() != ys.len() {
    err("length mismatch: " + str(xs.len()) + " != " + str(ys.len()))
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
  assert.eq(std.type(xs), std.dictionary)
  assert.eq(std.type(ys), std.dictionary)
  let (missing-left, missing-right) = ((), ())
  let (aligned-left, aligned-right) = ((), ())
  let zipped = (:)
  for (kx, vx) in xs.pairs() {
    aligned-left.push(kx)
    if ys.keys().contains(kx) {
      let vy = ys.remove(kx)
      zipped.insert(kx, (vx, vy))
      aligned-right.push(kx)
    } else {
      missing-right.push(kx)
      aligned-right.push(none)
    }
  }
  for ky in ys.keys() {
    missing-left.push(ky)
    aligned-left.push(none)
    aligned-right.push(ky)
  }

  if missing-left.len() > 0 or missing-right.len() > 0 {
    err("dictionary key mismatch")
  } else {
    result-all-dict(
      ((vx, vy)) => f(vx, vy),
      zipped,
    )
  }
}

/// Returns the first ok result from mapping an array.
///
/// If every item fails, returns the first error.
/// -> RESULT(any)
#let result-any(
  /// Function returning a result for each item.
  /// -> function
  f,
  /// Items to try.
  /// -> array
  xs,
) = {
  let first-error = none
  for (i, x) in xs.enumerate() {
    let y = f(x)
    if y.__tag__ == "result/ok" {
      return y
    } else if y.__tag__ == "result/err" {
      if first-error == none {
        first-error = y
      }
    } else {
      panic("invalid result: `" + repr(y) + "`")
    }
  }
  if first-error == none {
    err("found no result")
  } else {
    first-error
  }
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
    panic(result.msg, result.trace)
  } else {
    panic("invalid result", result)
  }
}

/// Extracts the ok value using a custom error renderer.
///
/// The renderer receives the full error value.
/// -> any
#let result-unwrap-with(
  /// Converts an error result to a panic message.
  /// -> function
  error-to-string,
  /// Result to unwrap.
  /// -> RESULT(any)
  result,
) = {
  if result.__tag__ == "result/ok" {
    result.value
  } else if result.keys().contains("__tag__") {
    assert.eq(result.__tag__, "result/ok", message: error-to-string(result))
  } else {
    panic("invalid result", result)
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
