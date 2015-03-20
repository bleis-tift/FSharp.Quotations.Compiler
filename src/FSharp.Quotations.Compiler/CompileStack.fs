namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations
open System.Reflection.Emit
open System.Collections.Generic

type CompileStackInfo =
  | CompileTarget of Expr
  | Compiling of (ILGenerator -> unit)

type CompileStack = Stack<CompileStackInfo>