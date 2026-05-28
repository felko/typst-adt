#import "result.typ": *

#let enum-pop-tag(
  value,
) = {
  let tag = value.remove("__tag__").split("/").last()
  (value, tag)
}

/// Dispatches on an argument spec.
/// -> function
#let args-spec-elim(
  /// Case for no arguments.
  /// -> function | any | auto
  null: auto,
  /// Case for positional and named arguments.
  /// -> function | any | auto
  args: auto,
) = args-spec => {
  if null == auto and args == auto {
    panic("missing cases: `null`, `args`")
  } else if null == auto {
    panic("missing case: `null`")
  } else if args == auto {
    panic("missing case: `args`")
  }
  if args-spec == none {
    ok((__tag__: "args-spec/null"))
  } else if type(args-spec) == dictionary {
    if args-spec.keys().contains("__tag__") {
      if args-spec.__tag__.starts-with("args-spec/") {
        let tag = args-spec.remove("__tag__")
        if tag == "args-spec/null" {
          if args-spec.len() == 0 {
            null()
          } else {
            err(
              "too many fields in `args-spec/null`: "
                + constr-spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else if tag == "args-spec/args" {
          if not args-spec.keys().contains("pos") {
            return err("missing `pos` fields in `args-spec/args`")
          }
          let pos = args-spec.remove("pos")

          if not args-spec.keys().contains("named") {
            return err("missing `named` field in `args-spec/args`")
          }
          let named = args-spec.remove("named")

          if args-spec.len() == 0 {
            args(..pos, ..named)
          } else {
            err(
              "too many fields in `args-spec/args`: "
                + args-spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        }
      } else {
        args(args-spec)
      }
    }
  } else if type(args-spec) == arguments {
    let pos = result-all(spec-parse, args-spec.pos())
    let named = result-all-dict(spec-parse, args-spec.named())
    (
      __tag__: "args-spec/args",
      pos: pos,
      named: named,
    )
  } else {
    panic("invalid args spec: `" + repr(args-spec) + "`")
  }
}

/// Dispatches on a constructor spec.
///
/// Pass `null:` and `fields:` cases. Cases may be functions or constants.
/// -> function
#let constr-spec-elim(
  /// Case for constructors with no fields.
  /// -> function | any | auto
  null: auto,
  /// Case for constructors with fields.
  /// -> function | any | auto
  fields: auto,
) = constr-spec => {
  if null == auto and fields == auto {
    panic("missing cases: `null`, `fields`")
  } else if null == auto {
    panic("missing case: `null`")
  } else if fields == auto {
    panic("missing case: `fields`")
  }
  if constr-spec == none {
    ok((__tag__: "constr-spec/null"))
  } else if type(constr-spec) == arguments {
    if constr-spec.pos().len() == 0 {
      result-map(
        fields => (__tag__: "constr-spec/fields", fields: fields),
        result-all-dict(spec-parse, constr-spec.named()),
      )
    } else if constr-spec.pos().len() == 1 and constr-spec.named().len() == 0 {
      result-map(
        spec => (
          __tag__: "constr-spec/fields",
          fields: (value: spec),
        ),
        spec-parse(constr-spec.pos().first()),
      )
    } else {
      err(
        "constructor field specifications must be named or a single positional value spec",
      )
    }
  } else if type(constr-spec) == dictionary {
    if constr-spec.keys().contains("__tag__") {
      if constr-spec.__tag__.starts-with("constr-spec/") {
        let tag = constr-spec.remove("__tag__")
        if tag == "constr-spec/null" {
          if constr-spec.len() == 0 {
            if type(null) == type(() => none) {
              null()
            } else {
              null
            }
          } else {
            err(
              "too many fields in `constr-spec/null`: "
                + constr-spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else if tag == "constr-spec/fields" {
          if not constr-spec.keys().contains("fields") {
            return err("missing `fields` field in `args-spec/args`")
          }
          let fields_ = constr-spec.remove("fields")

          if constr-spec.len() == 0 {
            fields(fields_)
          } else {
            err(
              "too many fields in `constr-spec/fields`: "
                + constr-spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else {
          panic("unknown constructor spec kind: `" + tag + "`")
        }
      } else {
        panic("invalid constructor spec tag: `" + constr-spec.__tag__ + "`")
      }
    } else {
      panic("dictionary constructor specs must contain `__tag__`")
    }
  } else {
    panic("invalid constr spec: `" + repr(constr-spec) + "`")
  }
}

/// Dispatches on a spec kind.
///
/// All spec cases must be provided. This is the central eliminator for spec
/// values.
/// -> function
#let spec-elim(
  /// Case for `adt.empty`.
  /// -> function
  empty_case: auto,
  /// Case for builtin specs.
  /// -> function
  builtin: auto,
  /// Case for `adt.any`.
  /// -> function
  any: auto,
  /// Case for union specs.
  /// -> function
  union_case: auto,
  /// Case for enum specs.
  /// -> function
  enum: auto,
  /// Case for struct specs.
  /// -> function
  struct: auto,
  /// Case for array specs.
  /// -> function
  array: auto,
  /// Case for dictionary specs.
  /// -> function
  dict: auto,
  /// Case for function specs.
  /// -> function
  function: auto,
  /// Case for fixed-point specs.
  /// -> function
  fix: auto,
  /// Case for recursive self refs.
  /// -> function
  self: auto,
) = spec => {
  let missing-cases = (
    empty: empty_case,
    builtin: builtin,
    any: any,
    union: union_case,
    enum: enum,
    struct: struct,
    array: array,
    dictionary: dict,
    function: function,
    self: self,
    fix: fix,
  )
    .pairs()
    .filter(((k, v)) => v == auto)
    .map(p => p.at(0))
  if missing-cases.len() == 1 {
    panic("missing case: `" + missing-cases.first() + "`")
  } else if missing-cases.len() > 1 {
    panic(
      "missing cases: "
        + missing-cases.map(case => "`" + case + "`").join(", "),
    )
  }
  if type(spec) == type {
    builtin(spec)
  } else if type(spec) == dictionary {
    if spec.keys().contains("__tag__") and spec.__tag__.starts-with("spec/") {
      let tag = spec.remove("__tag__")
      if tag == "spec/empty" {
        if empty_case == auto {
          panic("missing case: `empty`")
        }
        if spec.len() == 0 {
          empty_case()
        } else {
          panic(
            "too many fields in `spec/empty`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/builtin" {
        let _ = spec.remove("name", default: none)
        let value = spec.remove("value")
        if type(value) == type {
          if spec.len() == 0 {
            builtin(value)
          } else {
            panic(
              "too many fields in `spec/builtin`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else {
          panic("expected type in `spec/builtin`, got `" + repr(value) + "`")
        }
      } else if tag == "spec/any" {
        if type(any) == type(() => none) {
          any()
        } else {
          any
        }
      } else if tag == "spec/enum" {
        let name = spec.remove("name", default: auto)
        let constrs = spec.remove("constrs")
        if type(constrs) == dictionary {
          if spec.len() == 0 {
            enum(name, constrs)
          } else {
            panic(
              "too many fields in `spec/enum`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else {
          panic(
            "expected dictionary for `spec/enum` constructors, got `"
              + repr(constrs)
              + "`",
          )
        }
      } else if tag == "spec/union" {
        if union_case == auto {
          panic("missing case: `union`")
        }
        let name = spec.remove("name", default: auto)
        let elems = spec.remove("elems")
        if type(elems) == type(()) {
          if spec.len() == 0 {
            union_case(name, elems)
          } else {
            panic(
              "too many fields in `spec/union`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else {
          panic(
            "expected array for `spec/union` elements, got `"
              + str(type(elems))
              + "`",
          )
        }
      } else if tag == "spec/struct" {
        let name = spec.remove("name", default: auto)
        let fields = spec.remove("fields")
        if type(fields) == dictionary {
          let _ = spec.remove("__name__", default: none)
          if spec.len() == 0 {
            struct(name, fields)
          } else {
            panic(
              "too many fields in `spec/struct`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else {
          panic(
            "expected dictionary for `spec/struct` fields, got `"
              + repr(fields)
              + "`",
          )
        }
      } else if tag == "spec/array" {
        if array == auto {
          panic("missing case: `array`")
        }
        let name = spec.remove("name", default: auto)
        let inner = spec.remove("inner")
        if spec.len() == 0 {
          array(name, inner)
        } else {
          panic(
            "too many fields in `spec/array`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/dict" {
        if dict == auto {
          panic("missing case: `dictionary`")
        }
        let name = spec.remove("name", default: auto)
        let value = spec.remove("value")
        if spec.len() == 0 {
          dict(name, value)
        } else {
          panic(
            "too many fields in `spec/dict`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/function" {
        if function == auto {
          panic("missing case: `function`")
        }
        let name = spec.remove("name", default: auto)
        let dom = spec.remove("dom")
        let cod = spec.remove("cod")
        if spec.len() == 0 {
          function(name, dom, cod)
        } else {
          panic(
            "too many fields in `spec/function`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/fix" {
        if not spec.keys().contains("fun") {
          panic("expected `fun` field for `spec/fix`, got `" + repr(spec) + "`")
        } else {
          let name = spec.remove("name", default: auto)
          let fun = spec.remove("fun")
          if spec.len() == 0 {
            fix(name, fun)
          } else {
            panic(
              "too many fields in `spec/fix`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        }
      } else {
        panic("unknown spec kind: `" + tag + "`")
      }
    } else {
      panic("dictionary specs must contain a `spec/` tag")
    }
  } else {
    panic("ill-formed spec: `" + repr(spec) + "`")
  }
}

/// Renders an argument spec with a custom spec renderer.
///
/// Used by `args-spec-to-string` and `spec-to-string`.
/// -> str
#let args-spec-to-string-aux(
  /// Spec renderer.
  /// -> function
  spec-to-string,
  /// Argument spec.
  /// -> args-spec
  args-spec,
) = {
  if args-spec.__tag__ == "args-spec/null" {
    ""
  } else if args-spec.__tag__ == "args-spec/args" {
    (
      "("
        + (
          args-spec.pos.map(arg-spec => spec-to-string(arg-spec))
            + args-spec
              .named
              .pairs()
              .map(((arg-name, arg-spec)) => {
                arg-name + ": " + spec-to-string(arg-spec)
              })
        ).join(", ")
        + ")"
    )
  } else {
    panic("ill-formed arguments spec: `" + repr(args-spec) + "`")
  }
}

/// Renders a constructor spec with a custom spec renderer.
///
/// Used by `spec-to-string` for enum constructors.
/// -> str
#let constr-spec-to-string-aux(
  /// Spec renderer.
  /// -> function
  spec-to-string,
  /// Constructor spec.
  /// -> constr-spec
  constr-spec,
) = {
  if constr-spec.__tag__ == "constr-spec/null" {
    ""
  } else if constr-spec.__tag__ == "constr-spec/fields" {
    (
      "("
        + (
          constr-spec
            .fields
            .pairs()
            .map(((arg-name, arg-spec)) => {
              arg-name + ": " + spec-to-string(arg-spec)
            })
        ).join(", ")
        + ")"
    )
  } else {
    panic("ill-formed constructor spec: `" + repr(constr-spec) + "`")
  }
}

/// Parses shorthand argument specs into explicit argument specs.
///
/// This lower-level variant accepts the recursive spec parser to use.
/// -> RESULT(args-spec)
#let args-spec-parse-aux(
  /// Spec parser to use recursively.
  /// -> function
  spec-parse,
  /// Argument spec shorthand.
  /// -> any
  args-spec,
) = {
  if args-spec == none {
    ok((__tag__: "args-spec/null"))
  } else if type(args-spec) == dictionary {
    if args-spec.keys().contains("__tag__") {
      if args-spec.__tag__.starts-with("args-spec/") {
        let tag = args-spec.remove("__tag__")
        if tag == "args-spec/null" {
          if args-spec.len() == 0 {
            ok((__tag__: "args-spec/null"))
          } else {
            err(
              "too many fields in `args-spec/null`: "
                + constr-spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else if tag == "args-spec/args" {
          if not args-spec.keys().contains("pos") {
            return err("missing `pos` fields in `args-spec/args`")
          }
          let pos = args-spec.remove("pos")
          pos = result-all(spec-parse, pos)
          if result-is-err(pos) {
            return pos
          } else {
            pos = pos.value
          }

          if not args-spec.keys().contains("named") {
            return err("missing `named` fields in `args-spec/args`")
          }
          let named = args-spec.remove("named")
          named = result-all-dict(spec-parse, named)
          if result-is-err(named) {
            return named
          } else {
            named = named.value
          }

          if args-spec.len() == 0 {
            ok((
              __tag__: "args-spec/args",
              pos: pos,
              named: named,
            ))
          } else {
            err(
              "too many fields in `args-spec/args`: "
                + constr-spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        }
      } else {}
    }
  } else if type(args-spec) == arguments {
    let pos = result-all(spec-parse, args-spec.pos())
    let named = result-all-dict(spec-parse, args-spec.named())
    result-map2(
      (pos, named) => (
        __tag__: "args-spec/args",
        pos: pos,
        named: named,
      ),
      pos,
      named,
    )
  } else {}
}

/// Parses shorthand constructor specs into explicit constructor specs.
///
/// This lower-level variant accepts the recursive spec parser to use.
/// -> RESULT(constr-spec)
#let constr-spec-parse-aux(
  /// Spec parser to use recursively.
  /// -> function
  spec-parse,
  /// Constructor spec shorthand.
  /// -> any
  constr-spec,
) = {
  if constr-spec == none {
    ok((__tag__: "constr-spec/null"))
  } else if type(constr-spec) == dictionary {
    if (
      constr-spec.keys().contains("__tag__")
        and constr-spec.__tag__.starts-with("constr-spec/")
    ) {
      let tag = constr-spec.remove("__tag__")
      if tag == "constr-spec/null" {
        if constr-spec.len() == 0 {
          ok((__tag__: "constr-spec/null"))
        } else {
          err(
            "too many fields in `constr-spec/null`: "
              + constr-spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "constr-spec/fields" {
        if constr-spec.keys().contains("fields") {
          let fields = constr-spec.remove("fields")
          if constr-spec.len() > 0 {
            err(
              "too many fields for `constr-spec/fields`: "
                + constr-spec.keys().map(k => "`" + k + "`").join(", "),
            )
          } else {
            result-map(
              fields => (__tag__: tag, fields: fields),
              result-all-dict(
                spec-parse,
                fields,
              ),
            )
          }
        }
      }
    } else {
      let result = result-map(
        fields => (__tag__: "constr-spec/fields", fields: fields),
        result-all-dict(
          spec-parse,
          constr-spec,
        ),
      )
      if result.__tag__ == "result/ok" {
        ok(result.value)
      } else if result.__tag__ == "result/err" {
        result-map(
          spec => (
            __tag__: "constr-spec/fields",
            fields: (value: spec),
          ),
          spec-parse(constr-spec),
        )
      } else {
        panic("invalid result: `" + repr(result) + "`")
      }

      // otherwise, the spec describes named constructor arguments
    }
  } else {
    result-map(
      spec => (
        __tag__: "constr-spec/fields",
        fields: (value: spec),
      ),
      spec-parse(constr-spec),
    )
  }
}

/// Parses shorthand specs into explicit specs.
///
/// Builtin types become builtin specs, plain dictionaries become struct specs,
/// and nested specs are parsed recursively.
/// -> RESULT(spec)
#let spec-parse(
  /// Spec shorthand.
  /// -> any
  spec,
) = {
  if type(spec) == type {
    ok((__tag__: "spec/builtin", name: str(spec), value: spec))
  } else if type(spec) == type(() => none) {
    ok(spec)
  } else if type(spec) == dictionary {
    if spec.keys().contains("__tag__") and spec.__tag__.starts-with("spec/") {
      let tag = spec.remove("__tag__")
      if tag == "spec/empty" {
        if spec.len() == 0 {
          ok((__tag__: "spec/empty"))
        } else {
          err(
            "too many fields in `spec/empty`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/builtin" {
        let name = spec.remove("name", default: auto)
        let value = spec.remove("value")
        if type(value) == type {
          if spec.len() == 0 {
            ok((__tag__: "spec/builtin", name: name, value: value))
          } else {
            err(
              "too many fields in `spec/builtin`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else {
          err("expected type in `spec/builtin`, got `" + repr(value) + "`")
        }
      } else if tag == "spec/any" {
        if spec.len() == 0 {
          ok((__tag__: "spec/any"))
        } else {
          err(
            "too many fields in `spec/any`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/enum" {
        let name = spec.remove("name", default: auto)
        let constrs = spec.remove("constrs")
        if type(constrs) == dictionary {
          result-map(
            constrs => (__tag__: "spec/enum", name: name, constrs: constrs),
            result-all-dict(
              args-spec => constr-spec-parse-aux(spec-parse, args-spec),
              constrs,
            ),
          )
        } else {
          err(
            "expected dictionary for `spec/enum` constructors, got `"
              + repr(constrs)
              + "`",
          )
        }
      } else if tag == "spec/union" {
        let name = spec.remove("name", default: auto)
        let elems = spec.remove("elems")
        if type(elems) == type(()) {
          if spec.len() == 0 {
            result-map(
              elems => (__tag__: "spec/union", name: name, elems: elems),
              result-all(spec-parse, elems),
            )
          } else {
            err(
              "too many fields in `spec/union`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else {
          err(
            "expected array for `spec/union` elements, got `"
              + repr(elems)
              + "`",
          )
        }
      } else if tag == "spec/array" {
        let name = spec.remove("name", default: auto)
        let inner = spec.remove("inner")
        if spec.len() == 0 {
          result-map(
            inner => (__tag__: "spec/array", name: name, inner: inner),
            spec-parse(inner),
          )
        } else {
          err(
            "too many fields in `spec/array`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/dict" {
        let name = spec.remove("name", default: auto)
        let value = spec.remove("value")
        if spec.len() == 0 {
          result-map(
            value => (
              __tag__: "spec/dict",
              name: name,
              value: value,
            ),
            spec-parse(value),
          )
        } else {
          err(
            "too many fields in `spec/dict`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/function" {
        let name = spec.remove("name", default: auto)
        let dom = spec.remove("dom")
        let cod = spec.remove("cod")
        if spec.len() == 0 {
          result-map2(
            (dom, cod) => (
              __tag__: "spec/function",
              name: name,
              dom: dom,
              cod: cod,
            ),
            args-spec-parse-aux(spec-parse, dom),
            spec-parse(cod),
          )
        } else {
          err(
            "too many fields in `spec/function`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/fix" {
        if not spec.keys().contains("fun") {
          err("expected `fun` field for `spec/fix`, got `" + repr(spec) + "`")
        } else {
          let name = spec.remove("name", default: auto)
          let fun = spec.remove("fun")
          if spec.len() == 0 {
            ok((__tag__: "spec/fix", name: name, fun: fun))
          } else {
            err(
              "too many fields in `spec/fix`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        }
      } else if tag == "spec/self" {
        ok((__tag__: "spec/self", depth: spec.depth))
      } else {
        err("unknown spec kind: `" + repr(tag) + "`")
      }
    } else {
      let name = spec.remove("name", default: spec.remove(
        "__name__",
        default: auto,
      ))
      result-map(
        fields => (
          __tag__: "spec/struct",
          name: name,
          fields: fields,
        ),
        result-all-dict(spec-parse, spec),
      )
    }
  } else {
    err("ill-formed spec: `" + repr(spec) + "`")
  }
}

/// Renders a spec as a compact string.
///
/// Shorthand specs are parsed first, so builtin types such as `int` work
/// directly.
/// -> str
#let to-string(
  /// Spec or spec shorthand.
  /// -> any
  spec,
  /// Reserved precedence parameter.
  /// -> int
  prec: 0,
  /// Recursive self depth.
  /// -> int
  depth: 0,
) = {
  spec = result-unwrap(spec-parse(spec))
  let existing-name = if type(spec) == dictionary {
    spec.at("name", default: spec.at(
      "__name__",
      default: auto,
    ))
  } else {
    auto
  }
  if existing-name != auto {
    return existing-name
  }
  spec-elim(
    empty_case: () => "empty",
    builtin: type_ => str(type_),
    any: () => "any",
    union_case: (name, elems) => elems
      .map(elem => to-string(elem, depth: depth))
      .join(" | "),
    enum: (name, constrs) => (
      "enum {"
        + constrs
          .pairs()
          .map(((constr-name, constr-spec)) => {
            (
              constr-name
                + constr-spec-to-string-aux(
                  spec => to-string(spec, depth: depth),
                  constr-spec,
                )
            )
          })
          .join(", ")
        + "}"
    ),
    struct: (name, fields) => (
      "struct {"
        + fields
          .pairs()
          .map(((field-name, field-spec)) => {
            field-name + ": " + to-string(field-spec, depth: depth)
          })
          .join(", ")
        + "}"
    ),
    array: (name, inner) => (
      "array(" + to-string(inner, depth: depth) + ")"
    ),
    dict: (name, value) => (
      "dict(" + to-string(value, depth: depth) + ")"
    ),
    function: (name, dom, cod) => (
      args-spec-to-string-aux(spec => to-string(spec, depth: depth), dom)
        + " → "
        + to-string(cod, depth: depth)
    ),
    fix: (name, fun) => {
      let var = "self@" + str(depth)
      (
        "fix "
          + var
          + ". "
          + to-string(
            (fun)((
              __tag__: "spec/self",
              depth: depth,
            )),
            depth: depth + 1,
          )
      )
    },
    self: depth => "self@" + str(depth),
  )(spec)
}

/// Renders an argument spec as a compact string.
/// -> function
#let args-spec-to-string = args-spec-to-string-aux.with(to-string)


/// Parses an argument spec using the default spec parser.
/// -> function
#let args-spec-parse = args-spec-parse-aux.with(spec-parse)

/// Parses a constructor spec using the default spec parser.
/// -> function
#let constr-spec-parse = constr-spec-parse-aux.with(spec-parse)
