namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations.Patterns

module Expr =
  let getMethodInfo = function
  | Call (_, mi, _) -> mi
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
