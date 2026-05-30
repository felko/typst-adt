#import "result.typ": *
#import "std.typ" as std

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
  } else if std.type(args-spec) == std.dictionary {
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
              "too many fields in `args-spec/args`: " + repr(args-spec.keys()),
            )
          }
        }
      } else {
        args(args-spec)
      }
    }
  } else if std.type(args-spec) == std.arguments {
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
  } else if std.type(constr-spec) == std.arguments {
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
  } else if std.type(constr-spec) == std.dictionary {
    if constr-spec.keys().contains("__tag__") {
      if constr-spec.__tag__.starts-with("constr-spec/") {
        let tag = constr-spec.remove("__tag__")
        if tag == "constr-spec/null" {
          if constr-spec.len() == 0 {
            if std.type(null) == std.function {
              null()
            } else {
              null
            }
          } else {
            err(
              "too many fields in `constr-spec/null`: " + repr(constr-spec.keys()),
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
              "too many fields in `constr-spec/fields`: " + repr(constr-spec.keys()),
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
  /// Case for builtin specs.
  /// -> function
  builtin: auto,
  /// Case for `adt.any`.
  /// -> function
  any: auto,
  /// Case for union specs.
  /// -> function
  union: auto,
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
  dictionary: auto,
  /// Case for function specs.
  /// -> function
  function: auto,
  /// Case for fixed-point specs.
  /// -> function
  fix: auto,
  /// Case for recursive self refs.
  /// -> function
  self: auto,
  /// Default case
  /// -> function
  __default__: auto,
) = spec => {
  let (builtin, any, union, enum, struct, array, dictionary, function, fix, self) = if __default__ == auto {
    let missing-cases = (
      builtin: builtin,
      any: any,
      union: union,
      enum: enum,
      struct: struct,
      array: array,
      dictionary: dictionary,
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
          + missing-cases.map(case => "`" + case + "`").join(", ") + "; either fill them in or specify a `__default__` case",
      )
    }
    (builtin, any, union, enum, struct, array, dictionary, function, fix, self)
  } else {
    (builtin, any, union, enum, struct, array, dictionary, function, fix, self).map(case => if case == auto { __default__ } else { case })
  }.map(case => if std.type(case) == std.function { case } else { (.. args) => case })
  if std.type(spec) == std.type {
    builtin(spec)
  } else if std.type(spec) == std.dictionary {
    if spec.keys().contains("__tag__") and spec.__tag__.starts-with("spec/") {
      let tag = spec.remove("__tag__")
      if tag == "spec/builtin" {
        let _ = spec.remove("name", default: none)
        let value = spec.remove("value")
        if std.type(value) == std.type {
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
        any()
      } else if tag == "spec/enum" {
        let name = spec.remove("name", default: auto)
        let constrs = spec.remove("constrs")
        if std.type(constrs) == std.dictionary {
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
            "expected dictionary for `spec/enum` constructors, got",
            constrs
          )
        }
      } else if tag == "spec/union" {
        if union == auto {
          panic("missing case: `union`")
        }
        let name = spec.remove("name", default: auto)
        let elems = spec.remove("elems")
        if std.type(elems) == std.array {
          if spec.len() == 0 {
            union(name, elems)
          } else {
            panic(
              "too many fields in `spec/union`: "
                + spec.keys().map(key => "`" + key + "`").join(", "),
            )
          }
        } else {
          panic(
            "expected array for `spec/union` elements, got `"
              + str(std.type(elems))
              + "`",
          )
        }
      } else if tag == "spec/struct" {
        let name = spec.remove("name", default: auto)
        let fields = spec.remove("fields")
        if std.type(fields) == std.dictionary {
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
            "expected dictionary for `spec/struct` fields, got",
            fields,
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
      } else if tag == "spec/dictionary" {
        let name = spec.remove("name", default: auto)
        let value = spec.remove("value")
        if spec.len() == 0 {
          dictionary(name, value)
        } else {
          panic(
            "too many fields in `spec/dict`: "
              + spec.keys().map(key => "`" + key + "`").join(", "),
          )
        }
      } else if tag == "spec/function" {
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
    panic("ill-formed spec", spec)
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
    panic("ill-formed arguments spec", args-spec)
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
    panic("ill-formed constructor spec ", constr-spec)
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
  } else if std.type(args-spec) == std.dictionary {
    if args-spec.keys().contains("__tag__") {
      if args-spec.__tag__.starts-with("args-spec/") {
        let tag = args-spec.remove("__tag__")
        if tag == "args-spec/null" {
          if args-spec.len() == 0 {
            ok((__tag__: "args-spec/null"))
          } else {
            err(
              "too many fields in `args-spec/null`: " + repr(constr-spec.keys()),
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
              "too many fields in `args-spec/args`: " + repr(constr-spec.keys()),
            )
          }
        }
      } else {}
    }
  } else if std.type(args-spec) == std.arguments {
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
  } else if std.type(constr-spec) == std.dictionary {
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
            "too many fields in `constr-spec/null`: " + repr(constr-spec.keys()),
          )
        }
      } else if tag == "constr-spec/fields" {
        if constr-spec.keys().contains("fields") {
          let fields = constr-spec.remove("fields")
          if constr-spec.len() > 0 {
            err(
              "too many fields for `constr-spec/fields`: " + repr(constr-spec.keys()),
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
  if std.type(spec) == std.type {
    ok((__tag__: "spec/builtin", name: str(spec), value: spec))
  } else if std.type(spec) == std.function {
    ok(spec)
  } else if std.type(spec) == std.dictionary {
    if spec.keys().contains("__tag__") and spec.__tag__.starts-with("spec/") {
      let tag = spec.remove("__tag__")
      if tag == "spec/empty" {
        if spec.len() == 0 {
          ok((__tag__: "spec/empty"))
        } else {
          err(
            "too many fields in `spec/empty`: " + repr(spec.keys()),
          )
        }
      } else if tag == "spec/builtin" {
        let name = spec.remove("name", default: auto)
        let value = spec.remove("value")
        if std.type(value) == std.type {
          if spec.len() == 0 {
            ok((__tag__: "spec/builtin", name: name, value: value))
          } else {
            err(
              "too many fields in `spec/builtin`: " + repr(spec.keys()),
            )
          }
        } else {
          err("expected type in `spec/builtin`, got " + repr(value))
        }
      } else if tag == "spec/any" {
        if spec.len() == 0 {
          ok((__tag__: "spec/any"))
        } else {
          err(
            "too many fields in `spec/any`: " + repr(spec.keys()),
          )
        }
      } else if tag == "spec/enum" {
        let name = spec.remove("name", default: auto)
        let constrs = spec.remove("constrs")
        if std.type(constrs) == std.dictionary {
          result-map(
            constrs => (__tag__: "spec/enum", name: name, constrs: constrs),
            result-all-dict(
              args-spec => constr-spec-parse-aux(spec-parse, args-spec),
              constrs,
            ),
          )
        } else {
          err(
            "expected dictionary for `spec/enum` constructors, got " + repr(constrs),
          )
        }
      } else if tag == "spec/union" {
        let name = spec.remove("name", default: auto)
        let elems = spec.remove("elems")
        if std.type(elems) == std.array {
          if spec.len() == 0 {
            result-map(
              elems => (__tag__: "spec/union", name: name, elems: elems),
              result-all(spec-parse, elems),
            )
          } else {
            err(
              "too many fields in `spec/union`: " + repr(spec.keys()),
            )
          }
        } else {
          err(
            "expected array for `spec/union` elements, got " + repr(elems),
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
            "too many fields in `spec/array`: " + repr(spec.keys()),
          )
        }
      } else if tag == "spec/dictionary" {
        let name = spec.remove("name", default: auto)
        let value = spec.remove("value")
        if spec.len() == 0 {
          result-map(
            value => (
              __tag__: "spec/dictionary",
              name: name,
              value: value,
            ),
            spec-parse(value),
          )
        } else {
          err(
            "too many fields in `spec/dict`: " + repr(spec.keys()),
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
            "too many fields in `spec/function`: " + repr(spec.keys()),
          )
        }
      } else if tag == "spec/fix" {
        if not spec.keys().contains("fun") {
          err("expected `fun` field for `spec/fix`: " + repr(spec))
        } else {
          let name = spec.remove("name", default: auto)
          let fun = spec.remove("fun")
          if spec.len() == 0 {
            ok((__tag__: "spec/fix", name: name, fun: fun))
          } else {
            err(
              "too many fields in `spec/fix`: " + repr(spec.keys()),
            )
          }
        }
      } else if tag == "spec/self" {
        ok((__tag__: "spec/self", depth: spec.depth))
      } else {
        err("unknown spec kind: " + repr(tag))
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
    err("ill-formed spec: " + repr(spec))
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
  let existing-name = if std.type(spec) == std.dictionary {
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
    builtin: type_ => str(type_),
    any: () => "any",
    union: (name, elems) => elems
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
    dictionary: (name, value) => (
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

/// Renders a value without depending on imported `repr` helpers.
/// -> str
#let diagnostic-value-to-string = std.repr

/// Indents every line in a diagnostic block.
/// -> str
#let diagnostic-indent(text, prefix: "      ") = (
  text
    .split("\n")
    .map(line => prefix + line)
    .join("\n")
)

/// Renders one GHC-style diagnostic bullet.
/// -> str
#let diagnostic-bullet(title, body: none) = {
  let bullet = "  • " + title
  if body == none {
    bullet
  } else {
    bullet + "\n" + diagnostic-indent(body)
  }
}

/// Renders a structured validation trace.
///
/// More specific frames are printed first, followed by the outer context.
/// -> str
#let trace-to-string(trace) = {
  if std.type(trace) != std.dictionary or not trace.keys().contains("__tag__") {
    panic("invalid trace", trace)
  }
  let tag = trace.__tag__
  if tag == "trace/root" {
    return ""
  }

  let current = if tag == "trace/val" {
    diagnostic-bullet(
      "when checking the value:",
      body: diagnostic-value-to-string(trace.value)
        + "\n"
        + "against the expected type:\n"
        + "  "
        + to-string(trace.spec),
    )
  } else if tag == "trace/constr-null" {
    diagnostic-bullet(
      "the constructor does not accept arguments, but received:",
      body: diagnostic-value-to-string(trace.value),
    )
  } else if tag == "trace/constr-field" {
    diagnostic-bullet(
      "in constructor field `" + trace.constr-arg + "`:",
      body: diagnostic-value-to-string(trace.value),
    )
  } else if tag == "trace/constr" {
    diagnostic-bullet(
      "while validating constructor `" + trace.name + "`.",
    )
  } else if tag == "trace/constr-missing-arg" {
    diagnostic-bullet(
      "the constructor is missing argument `" + trace.constr-arg + "`.",
    )
  } else if tag == "trace/constr-extra-args" {
    diagnostic-bullet(
      "the constructor received extra arguments:",
      body: diagnostic-value-to-string(trace.extra-args),
    )
  } else if tag == "trace/constr-missing-field" {
    diagnostic-bullet(
      "the constructor value is missing field `" + trace.constr-arg + "`.",
    )
  } else if tag == "trace/args-null" {
    diagnostic-bullet(
      "the function does not accept arguments, but received:",
      body: diagnostic-value-to-string(trace.extra-args),
    )
  } else if tag == "trace/args-arity" {
    diagnostic-bullet(
      "the function arguments do not match the expected call shape:",
      body: diagnostic-value-to-string(trace.value)
        + "\n"
        + "expected:\n"
        + "  "
        + args-spec-to-string-aux(to-string, trace.args-spec),
    )
  } else if tag == "trace/args-extra-named" {
    diagnostic-bullet(
      "the function received unexpected named arguments:",
      body: diagnostic-value-to-string(trace.extra-args),
    )
  } else if tag == "trace/args-missing-named" {
    diagnostic-bullet(
      "the function is missing named arguments:",
      body: trace.missing-args.map(name => "`" + name + "`").join(", "),
    )
  } else if tag == "trace/args-pos-arg" {
    diagnostic-bullet(
      "in positional argument " + str(trace.index) + ":",
      body: diagnostic-value-to-string(trace.value)
        + "\n"
        + "of call shape:\n"
        + "  "
        + args-spec-to-string-aux(to-string, trace.args-spec),
    )
  } else if tag == "trace/args-named-arg" {
    diagnostic-bullet(
      "in named argument `" + trace.name + "`:",
      body: diagnostic-value-to-string(trace.value)
        + "\n"
        + "of call shape:\n"
        + "  "
        + args-spec-to-string-aux(to-string, trace.args-spec),
    )
  } else if tag == "trace/array-val" {
    diagnostic-bullet(
      "in array element at index " + str(trace.index) + ":",
      body: diagnostic-value-to-string(trace.value),
    )
  } else if tag == "trace/dictionary-val" {
    diagnostic-bullet(
      "in dictionary value at key `" + trace.key + "`:",
      body: diagnostic-value-to-string(trace.value),
    )
  } else if tag == "trace/union" {
    diagnostic-bullet(
      "while trying union member `" + to-string(trace.spec) + "`:",
      body: diagnostic-value-to-string(trace.value),
    )
  } else {
    panic("invalid trace tag", tag)
  }

  if trace.keys().contains("cont") {
    let rest = trace-to-string(trace.cont)
    if rest == "" {
      current
    } else {
      rest + "\n" + current
    }
  } else {
    current
  }
}

/// Renders an error result as a GHC-style diagnostic.
/// -> str
#let result-error-to-string(result) = {
  if result.__tag__ != "result/err" {
    panic("expected an error result", result)
  }
  let trace = trace-to-string(result.trace)
  (
    "error:\n"
      + diagnostic-bullet(result.msg)
      + if trace == "" { "" } else { "\n" + trace }
  )
}

/// Extracts an ok value and prints structured errors as diagnostics.
/// -> any
#let pretty-result-unwrap = result-unwrap-with.with(result-error-to-string)
