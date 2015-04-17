(*
 * FSharp.Quotations.Compiler - a compiler for F# expression tree
 * Written in 2015 by bleis-tift (hey_c_est_la_vie@hotmail.co.jp)
 * kyonmm, zakky-dev
 * 
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain worldwide.
 * This software is distributed without any warranty.
 * 
 * You should have received a copy of the CC0 Public Domain Dedication along with this software.
 * If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
 *)
namespace FSharp.Quotations.Compiler

open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.NullableOperators

module internal NullableOpTranslator =

  let (|Op|_|) expr altOpExpr (mi: MethodInfo) =
    let genMethod = Expr.getGenericMethodInfo expr
    if genMethod = mi then
      let altGenOp = Expr.getGenericMethodInfo altOpExpr
      Some altGenOp
    else
      None

  let transIfNeed (mi: MethodInfo) (argsExprs: Expr list) (stack: CompileStack) =
    if mi.IsGenericMethod then
      match mi.GetGenericMethodDefinition() with
      | Op <@ (?-) @> <@ (-) @> op
      | Op <@ (?/) @> <@ (/) @> op
      | Op <@ (?%) @> <@ (%) @> op ->
          let op = op.MakeGenericMethod(mi.GetGenericArguments())
          let lhs = argsExprs.[0] 
          let rhs = argsExprs.[1]
          let tmp = Var("$tmp", lhs.Type)
          let hasValue = lhs.Type.GetProperty("HasValue")
          let value = lhs.Type.GetProperty("Value")
          let expr =
            Expr.Let(
              tmp,
              lhs,
              Expr.IfThenElse(
                Expr.PropertyGet(Expr.Var(tmp), hasValue),
                Expr.NewObject(
                  mi.ReturnType.GetConstructor([|op.ReturnType|]),
                  [Expr.Call(op, [Expr.PropertyGet(Expr.Var(tmp), value); rhs])]),
                Expr.DefaultValue(mi.ReturnType)
            ))
          stack.Push(CompileTarget expr)
          true
      | Op <@ (-?) @> <@ (-) @> op
      | Op <@ (/?) @> <@ (/) @> op
      | Op <@ (%?) @> <@ (%) @> op ->
          let op = op.MakeGenericMethod(mi.GetGenericArguments())
          let lhs = argsExprs.[0] 
          let rhs = argsExprs.[1]
          let tmp = Var("$tmp", rhs.Type)
          let hasValue = rhs.Type.GetProperty("HasValue")
          let value = rhs.Type.GetProperty("Value")
          let expr =
            Expr.Let(
              tmp,
              rhs,
              Expr.IfThenElse(
                Expr.PropertyGet(Expr.Var(tmp), hasValue),
                Expr.NewObject(
                  mi.ReturnType.GetConstructor([|op.ReturnType|]),
                  [Expr.Call(op, [lhs; Expr.PropertyGet(Expr.Var(tmp), value)])]),
                Expr.DefaultValue(mi.ReturnType)
            ))
          stack.Push(CompileTarget expr)
          true
      | Op <@ (?-?) @> <@ (-) @> op
      | Op <@ (?/?) @> <@ (/) @> op
      | Op <@ (?%?) @> <@ (%) @> op ->
          let op = op.MakeGenericMethod(mi.GetGenericArguments())
          let lhs = argsExprs.[0] 
          let rhs = argsExprs.[1]
          let tmp1 = Var("$tmp1", lhs.Type)
          let hasValue1 = lhs.Type.GetProperty("HasValue")
          let value1 = lhs.Type.GetProperty("Value")
          let tmp2 = Var("$tmp2", rhs.Type)
          let hasValue2 = rhs.Type.GetProperty("HasValue")
          let value2 = rhs.Type.GetProperty("Value")
          let expr =
            Expr.Let(
              tmp1,
              lhs,
              Expr.Let(
                tmp2,
                rhs,
                Expr.IfThenElse(
                  <@@ %%(Expr.PropertyGet(Expr.Var(tmp1), hasValue1)) && %%(Expr.PropertyGet(Expr.Var(tmp2), hasValue2)) @@>,
                  Expr.NewObject(
                    mi.ReturnType.GetConstructor([|op.ReturnType|]),
                    [Expr.Call(op, [Expr.PropertyGet(Expr.Var(tmp1), value1); Expr.PropertyGet(Expr.Var(tmp2), value2)])]),
                  Expr.DefaultValue(mi.ReturnType)
            )))
          stack.Push(CompileTarget expr)
          true
      | _ -> false
    else
      false