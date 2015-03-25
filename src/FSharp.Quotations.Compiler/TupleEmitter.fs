namespace FSharp.Quotations.Compiler

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection

module TupleEmitter =
  let private getMethod = function
  | Call (_, mi, _) -> mi
  | expr -> failwithf "expr is not Method call: %A" expr

  let private getTypeFromHandleM = getMethod <@ Type.GetTypeFromHandle(RuntimeTypeHandle()) @>
  let private makeTupleM = getMethod <@ FSharpValue.MakeTuple([||], typeof<int * int>) @>
  let private unboxGenericM = (getMethod <@ (null: obj) :?> string @>).GetGenericMethodDefinition()

  let private newTuple1 types = typedefof<Tuple<_>>.MakeGenericType(types).GetConstructor(types)
  let private newTuple2 types = typedefof<_ * _>.MakeGenericType(types).GetConstructor(types)
  let private newTuple3 types = typedefof<_ * _ * _>.MakeGenericType(types).GetConstructor(types)
  let private newTuple4 types = typedefof<_ * _ * _ * _>.MakeGenericType(types).GetConstructor(types)
  let private newTuple5 types = typedefof<_ * _ * _ * _ * _>.MakeGenericType(types).GetConstructor(types)
  let private newTuple6 types = typedefof<_ * _ * _ * _ * _ * _>.MakeGenericType(types).GetConstructor(types)
  let private newTuple7 types = typedefof<_ * _ * _ * _ * _ * _ * _>.MakeGenericType(types).GetConstructor(types)

  let private emitNewTuple (types: Type list) (gen: ILGeneratorWrapper) =
    let typesArr = Array.ofList types
    match typesArr with
    | [|_|] -> gen.Emit(Newobj (newTuple1 typesArr))
    | [|_; _|] -> gen.Emit(Newobj (newTuple2 typesArr))
    | [|_; _; _|] -> gen.Emit(Newobj (newTuple3 typesArr))
    | [|_; _; _; _|] -> gen.Emit(Newobj (newTuple4 typesArr))
    | [|_; _; _; _; _|] -> gen.Emit(Newobj (newTuple5 typesArr))
    | [|_; _; _; _; _; _|] -> gen.Emit(Newobj (newTuple6 typesArr))
    | [|_; _; _; _; _; _; _|] -> gen.Emit(Newobj (newTuple7 typesArr))
    | _greaterThen7 ->
        failwith "unsupported tuple type."

  let emit (elems: Expr list) (stack: CompileStack) =
    if elems.Length < 8 then
      let types = elems |> List.map (fun e -> e.Type)
      stack.Push(Compiling (emitNewTuple types))
      elems |> List.rev |> List.iter (fun argExpr -> stack.Push(CompileTarget argExpr))
    else
      // TODO : [performance issue] rewrite using tuple ctor
      let tupleType = FSharpType.MakeTupleType(elems |> List.map (fun e -> e.Type) |> List.toArray)
      stack.Push(Assumed (function
                          | IfRet, gen -> gen.Emit(Tailcall); gen.Emit(Call (Method (unboxGenericM.MakeGenericMethod(tupleType))))
                          | _, gen -> gen.Emit(Call (Method (unboxGenericM.MakeGenericMethod(tupleType))))))
      stack.Push(Compiling (fun gen ->
        gen.Emit(Ldtoken (TokType tupleType))
        gen.Emit(Call (Method getTypeFromHandleM))
        gen.Emit(Call (Method makeTupleM))
      ))
      stack.Push(CompileTarget (Expr.NewArray(typeof<obj>, elems |> List.map (fun e -> Expr.Coerce(e, typeof<obj>)))))