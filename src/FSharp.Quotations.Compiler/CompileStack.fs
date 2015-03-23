namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations
open System.Reflection.Emit
open System.Collections.Generic

type CompilingState =
  | NotYet of Expr
  | Done

type Assumption =
  | IfSequential
  | False

type CompileStackInfo =
  | CompileTarget of Expr
  | Assumed of (Assumption * ILGeneratorWrapper -> unit)
  | Assumption of Assumption
  | Compiling of (ILGeneratorWrapper -> unit)
  | CompilingIfThenElse of falseLabel:Label * ifEndLabel:Label * cond:CompilingState * truePart:CompilingState * falsePart:CompilingState
  | RestoreGen of ILGeneratorWrapper

type CompileStack = Stack<CompileStackInfo>