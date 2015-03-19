namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations

module ExprCompiler =

  let compile (expr: Expr<'T>) : 'T =
    failwith "not impl."