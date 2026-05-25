#import "result.typ": *

#let enum-pop-tag(value) = {
  let tag = value.remove("__tag__").split("/").last()
  (value, tag)
}

#let args-spec-elim(
  none_: auto,
  args: auto,
) = args-spec => {
  if none_ == auto and args == auto {
    panic("missing cases: `none_`, `args`")
  } else if none_ == auto {
    panic("missing case: `none_`")
  } else if args == auto {
    panic("missing case: `args`")
  }
  if args-spec == none {
    ok((__tag__: "args-spec/none"))
  } else if type(args-spec) == dictionary {
    if args-spec.keys().contains("__tag__") {
      if args-spec.__tag__.starts-with("args-spec/") {
        let tag = args-spec.remove("__tag__")
        if tag == "args-spec/none" {
          if args-spec.len() == 0 {
            none_()
          } else {
            err("too many fields in `args-spec/none`: " + constr-spec.keys().map(key => "`" + key + "`").join(", "))
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
            args(.. pos, .. named)
          } else {
            err("too many fields in `args-spec/args`: " + args-spec.keys().map(key => "`" + key + "`").join(", "))
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

#let constr-spec-elim(
  none_: auto,
  fields: auto,
) = constr-spec => {
  if none_ == auto and fields == auto {
    panic("missing cases: `none_`, `fields`")
  } else if none_ == auto {
    panic("missing case: `none_`")
  } else if fields == auto {
    panic("missing case: `fields`")
  }
  if constr-spec == none {
    ok((__tag__: "constr-spec/none"))
  } else if type(constr-spec) == dictionary {
    if constr-spec.keys().contains("__tag__") {
      if constr-spec.__tag__.starts-with("constr-spec/") {
        let tag = constr-spec.remove("__tag__")
        if tag == "constr-spec/none" {
          if constr-spec.len() == 0 {
            if type(none_) == function {
              none_()
            } else {
              none_
            }
          } else {
            err("too many fields in `constr-spec/none`: " + constr-spec.keys().map(key => "`" + key + "`").join(", "))
          }
        } else if tag == "constr-spec/fields" {
          if not constr-spec.keys().contains("fields") {
            return err("missing `fields` field in `args-spec/args`")
          }
          let fields_ = constr-spec.remove("fields")
          
          if constr-spec.len() == 0 {
            fields(fields_)
          } else {
            err("too many fields in `constr-spec/fields`: " + constr-spec.keys().map(key => "`" + key + "`").join(", "))
          }
        } else {
          panic("todo")
        }
      } else {
        let x = constr-spec
        panic("todo") 
      }
    } else {
      panic("todo")
    }
  } else {
    panic("invalid constr spec: `" + repr(constr-spec) + "`")
  }
}

#let spec-elim(
  builtin: auto,
  any: auto,
  enum: auto,
  struct: auto,
  fix: auto,
  self: auto,
) = spec => {
  let missing-cases = (
    builtin: builtin,
    any: any,
    enum: enum,
    struct: struct,
    self: self,
    fix: fix,
  ).pairs().filter(((k, v)) => v == auto).map(p => p.at(0))
  if missing-cases.len() == 1 {
    panic("missing case: `" + missing-cases.first() + "`")
  } else if missing-cases.len() > 1 {
    panic("missing cases: " + missing-cases.map(case => "`" + case + "`").join(", "))
  }
  if type(spec) == type {
    builtin(spec)
  } else if type(spec) == dictionary {
    if spec.keys().contains("__tag__") and spec.__tag__.starts-with("spec/") {
      let tag = spec.remove("__tag__")
      if tag == "spec/builtin" {
        let _ = spec.remove("name", default: none)
        let value = spec.remove("value")
        if type(value) == type {
          if spec.len() == 0 {
            builtin(value)
          } else {
            panic("too many fields in `spec/builtin`: " + spec.keys().map(key => "`" + key + "`").join(", "))
          }
        } else {
          panic("expected type in `spec/builtin`, got `" + repr(value) + "`")
        }
      } else if tag == "spec/any" {
        if type(any) == function {
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
            panic("too many fields in `spec/enum`: " + spec.keys().map(key => "`" + key + "`").join(", "))
          }
        } else {
          panic("expected dictionary for `spec/enum` constructors, got `" + repr(constrs) + "`")
        }
      } else if tag == "spec/struct" {
        let name = spec.remove("name", default: auto)
        let fields = spec.remove("fields")
        if type(fields) == dictionary {
          let _ = spec.remove("__name__", default: none)
          if spec.len() == 0 {
            struct(name, fields)
          } else {
            panic("too many fields in `spec/struct`: " + spec.keys().map(key => "`" + key + "`").join(", "))
          }
        } else {
          panic("expected dictionary for `spec/struct` fields, got `" + repr(fields) + "`")
        }
      } else if tag == "spec/fix" {
        if not spec.keys().contains("fun") {
          panic("expected `fun` field for `spec/fix`, got `" +  repr(spec) + "`")
        } else {
          let name = spec.remove("name", default: auto)
          let fun = spec.remove("fun")
          if spec.len() == 0 {
            fix(name, fun)
          } else {
            erpanicr("too many fields in `spec/fix`: " + spec.keys().map(key => "`" + key + "`").join(", "))
          }
        }
      } else {
        panic("unknown spec kind: `" + tag + "`")
      }
    } else {
      panic("todo")
    }
  } else {
    panic("ill-formed spec: `" + repr(spec) + "`")
  }
}

#let args-spec-to-string-aux(spec-to-string, args-spec) = {
  if args-spec.__tag__ == "args-spec/none" {
    ""
  } else if args-spec.__tag__ == "args-spec/args" {
    "(" + (
      args-spec.pos.map(arg-spec => spec-to-string(arg-spec)) +
      args-spec.named.pairs().map(((arg-name, arg-spec)) => {
        arg-name + ": " + spec-to-string(arg-spec)
      })
    ).join(", ") + ")"
  } else {
    panic("ill-formed arguments spec: `" + repr(args-spec) + "`")
  }
}

#let constr-spec-to-string-aux(spec-to-string, constr-spec) = {
  if constr-spec.__tag__ == "constr-spec/none" {
    ""
  } else if constr-spec.__tag__ == "constr-spec/fields" {
    "(" + (
      constr-spec.fields.pairs().map(((arg-name, arg-spec)) => {
        arg-name + ": " + spec-to-string(arg-spec)
      })
    ).join(", ") + ")"
  } else {
    panic("ill-formed constructor spec: `" + repr(constr-spec) + "`")
  }
}

#let spec-to-string(spec, prec: 0, depth: 0) = {
  if spec.at("__name__", default: auto) != auto {
    return spec.__name__
  }
  if spec.__tag__ == "spec/builtin" {
    str(spec.value)
  } else if spec.__tag__ == "spec/any" {
    "any"
  } else if spec.__tag__ == "spec/empty" {
    "empty"
  } else if spec.__tag__ == "spec/function" {
    "(" + (spec.dom.pos().map(spec-to-string) + spec.dom.named().map(((arg-name, arg-spec)) => arg-name + ": " + spec-to-string(arg-spec))).join(", ") + ") → " + spec-to-string(spec.cod) 
  } else if spec.__tag__ == "spec/enum" {
    "enum {" + spec.constrs.pairs().map(((constr-name, constr-spec)) => {
      constr-name + constr-spec-to-string-aux(spec-to-string, constr-spec)
    }).join(", ") + "}"
  } else if spec.__tag__ == "spec/struct" {
    "struct {" + spec.fields.pairs().map(((field-name, field-spec)) => {
      field-name + ": " + spec-to-string(field-spec)
    }).join(", ")
  } else if spec.__tag__ == "spec/fix" {
    let var = "self@" + str(depth)
    "fix " + var + ". " + spec-to-string(
      (spec.fun)((
        __tag__: "spec/self",
        depth: depth
      )),
      depth: depth + 1
    )
  } else if spec.__tag__ == "spec/self" {
    "self@" + str(spec.depth)
  } else {
    panic("ill-formed spec: `" + repr(spec) + "`")
  }
}

#let args-spec-to-string = args-spec-to-string-aux.with(spec-to-string)


#let args-spec-parse-aux(spec-parse, args-spec) = {
  if args-spec == none {
    ok((__tag__: "args-spec/none"))
  } else if type(args-spec) == dictionary {
    if args-spec.keys().contains("__tag__") {
      if args-spec.__tag__.starts-with("args-spec/") {
        let tag = args-spec.remove("__tag__")
        if tag == "args-spec/none" {
          if args-spec.len() == 0 {
            ok((__tag__: "args-spec/none"))
          } else {
            err("too many fields in `args-spec/none`: " + constr-spec.keys().map(key => "`" + key + "`").join(", "))
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
              named: named
            ))
          } else {
            err("too many fields in `args-spec/args`: " + constr-spec.keys().map(key => "`" + key + "`").join(", "))
          }
        }
      } else {
        
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

  }
}

#let constr-spec-parse-aux(spec-parse, constr-spec) = {
  if constr-spec == none {
    ok((__tag__: "constr-spec/none"))
  } else if type(constr-spec) == dictionary {
    if constr-spec.keys().contains("__tag__") and constr-spec.__tag__.starts-with("constr-spec/") {
      let tag = constr-spec.remove("__tag__")
      if tag == "constr-spec/none" {
        if constr-spec.len() == 0 {
          ok((__tag__: "constr-spec/none"))
        } else {
          err("too many fields in `constr-spec/none`: " + constr-spec.keys().map(key => "`" + key + "`").join(", "))
        }
      } else if tag == "constr-spec/fields" {
        if constr-spec.keys().contains("fields") {
          let fields = constr-spec.remove("fields")
          if constr-spec.len() > 0 {
            err("too many fields for `constr-spec/fields`: " + constr-spec.keys().map(k => "`" + k + "`").join(", "))
          } else {
            result-map(
              fields => (__tag__: tag, fields: fields),
              result-all-dict(
                spec-parse,
                fields
              )
            )
          }
        }
      }
    } else {
      let result = result-map(
        fields => (__tag__: "constr-spec/fields", fields: fields),
        result-all-dict(
          spec-parse,
          constr-spec
        )
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
  }
}

#let spec-parse(spec) = {
  if type(spec) == type {
    ok((__tag__: "spec/builtin", value: spec))
  } else if type(spec) == function {
    ok(spec)
  } else if type(spec) == dictionary {
    if spec.keys().contains("__tag__") and spec.__tag__.starts-with("spec/") {
      let tag = spec.remove("__tag__")
      if tag == "spec/builtin" {
        let value = spec.remove("value")
        if type(value) == type {
          if spec.len() == 0 {
            ok((__tag__: "spec/builtin", value: value))
          } else {
            err("too many fields in `spec/builtin`: " + spec.keys().map(key => "`" + key + "`").join(", "))
          }
        } else {
          err("expected type in `spec/builtin`, got `" + repr(value) + "`")
        }
      } else if tag == "spec/any" {
        if spec.len() == 0 {
            ok((__tag__: "spec/any"))
          } else {
            err("too many fields in `spec/any`: " + spec.keys().map(key => "`" + key + "`").join(", "))
          }
      } else if tag == "spec/enum" {
        let constrs = spec.remove("constrs")
        if type(constrs) == dictionary {
          result-map(
            constrs => (__tag__: "spec/enum", constrs: constrs),
            result-all-dict(
              args-spec => constr-spec-parse-aux(spec-parse, args-spec),
              constrs
            ),
          )
        } else {
          err("expected dictionary for `spec/enum` constructors, got `" + repr(constrs) + "`")
        }
      } else if tag == "spec/fix" {
        if not spec.keys().contains("fun") {
          err("expected `fun` field for `spec/fix`, got `" +  repr(spec) + "`")
        } else {
          let name = spec.remove("name", default: auto)
          let fun = spec.remove("fun")
          if spec.len() == 0 {
            ok((__tag__: "spec/fix", name: name, fun: fun))
          } else {
            err("too many fields in `spec/fix`: " + spec.keys().map(key => "`" + key + "`").join(", "))
          }
        }
      } else if tag == "spec/self" {
        ok((__tag__: "spec/self", depth: spec.depth))
      } else {
        err("unknown spec kind: `" + repr(tag) + "`")
      }
    } else {
      result-map(
        fields => (
          __tag__: "spec/struct",
          fields: fields
        ),
        result-all-dict(spec-parse, spec),
      )
    }
  } else {
    err("ill-formed spec: `" + repr(spec) + "`")
  }
}


#let args-spec-parse = args-spec-parse-aux.with(spec-parse)
#let constr-spec-parse = constr-spec-parse-aux.with(spec-parse)
