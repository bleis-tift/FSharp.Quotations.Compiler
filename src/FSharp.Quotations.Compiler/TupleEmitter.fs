namespace FSharp.Quotations.Compiler

open System
open Microsoft.FSharp.Quotations

module TupleEmitter =
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
    let types = elems |> List.map (fun e -> e.Type)
    stack.Push(Compiling (emitNewTuple types))
    elems |> List.rev |> List.iter (fun argExpr -> stack.Push(CompileTarget argExpr))
