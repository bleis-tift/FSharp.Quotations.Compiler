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

open Microsoft.FSharp.Quotations.Patterns

module Expr =
  let rec getMethodInfo = function
  | Call (_, mi, _) -> mi
  | Let (_, _, body) -> getMethodInfo body
  | Lambda (_, body) -> getMethodInfo body
  | expr -> failwithf "expr is not Method call: %A" expr

  let getPropertyInfo = function
  | Let (_, _, PropertyGet (_, pi, _))
  | PropertyGet (_, pi, _) -> pi
  | expr -> failwithf "expr is not property get: %A" expr

  let rec getGenericMethodInfo = function
  | Call (_, mi, _) -> mi.GetGenericMethodDefinition()
  | TryWith (_, _, _, _, Sequential (_, Call (_, mi, _))) -> mi.GetGenericMethodDefinition()
  | Lambda (_, body) -> getGenericMethodInfo body
  | expr -> failwithf "expr is not Method call: %A" expr
