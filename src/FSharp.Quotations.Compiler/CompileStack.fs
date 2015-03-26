namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations
open System.Reflection.Emit
open System.Collections.Generic

type CompilingState =
  | NotYet of Expr
  | Done

type Assumption =
  | IfSequential
  | IfRet
  | False

type CompileStackInfo =
  | CompileTarget of Expr
  | Assumed of (Assumption * ILGeneratorWrapper -> unit)
  | Assumption of Assumption
  | Compiling of (ILGeneratorWrapper -> unit)
  | RestoreGen of ILGeneratorWrapper

type CompileStack = Stack<CompileStackInfo>