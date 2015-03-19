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
  let private modIntIntInt = getMethod <@ 1 % 1 @>

  open Microsoft.FSharp.Core.Operators.Checked

  let private negOvfInt = getMethod <@ -(1) @>
  let private subOvfIntIntInt = getMethod <@ 1 - 1 @>
  let private mulOvfIntIntInt = getMethod <@ 1 * 1 @>

  let emit (mi: MethodInfo) isTailCall (gen: ILGenerator) =
    if mi = posInt then ()
    elif mi = negInt then gen.Emit(OpCodes.Neg)
    elif mi = subIntIntInt then gen.Emit(OpCodes.Sub)
    elif mi = divIntIntInt then gen.Emit(OpCodes.Div)
    elif mi = modIntIntInt then gen.Emit(OpCodes.Rem)
    elif mi = subOvfIntIntInt then gen.Emit(OpCodes.Sub_Ovf)
    elif mi = mulOvfIntIntInt then gen.Emit(OpCodes.Mul_Ovf)
    elif mi = negOvfInt then gen.Emit(OpCodes.Ldc_I4_M1); gen.Emit(OpCodes.Mul_Ovf) // -n -> n * -1
    else
      if isTailCall then
        gen.Emit(OpCodes.Tailcall)
      gen.EmitCall(OpCodes.Call, mi, null)