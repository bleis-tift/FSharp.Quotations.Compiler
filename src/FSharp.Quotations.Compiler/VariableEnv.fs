namespace FSharp.Quotations.Compiler

open System
open System.Reflection
open System.Reflection.Emit

type VariableInfo =
  | Arg of int
  | Local of LocalBuilder * string
  | Field of FieldInfo

type VariableEnv = (string * Type * VariableInfo) list
