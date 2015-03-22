namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations
open System.Reflection.Emit
open System.Collections.Generic

type CompilingState =
  | NotYet of Expr
  | Done

type CompileStackInfo =
  | CompileTarget of Expr
  | Compiling of (ILGeneratorWrapper -> unit)
  | CompilingIfThenElse of falseLabel:Label * ifEndLabel:Label * cond:CompilingState * truePart:CompilingState * falsePart:CompilingState
  | RestoreGen of ILGeneratorWrapper

type CompileStack = Stack<CompileStackInfo>