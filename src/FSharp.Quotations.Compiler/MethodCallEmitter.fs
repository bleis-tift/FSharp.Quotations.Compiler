namespace FSharp.Quotations.Compiler

open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Quotations.Patterns

module internal MethodCallEmitter =

  let private getMethod = function
  | Call (_, mi, _) -> mi
  | expr -> failwithf "expr is not Method call: %A" expr

  let private posInt = getMethod <@ +(1) @>
  let private negInt = getMethod <@ -(1) @>
  let private subIntIntInt = getMethod <@ 1 - 1 @>
  let private divIntIntInt = getMethod <@ 1 / 1 @>

  let emit (mi: MethodInfo) isTailCall (gen: ILGenerator) =
    if mi = posInt then ()
    elif mi = negInt then gen.Emit(OpCodes.Neg)
    elif mi = subIntIntInt then gen.Emit(OpCodes.Sub)
    elif mi = divIntIntInt then gen.Emit(OpCodes.Div)
    else
      if isTailCall then
        gen.Emit(OpCodes.Tailcall)
      gen.EmitCall(OpCodes.Call, mi, null)