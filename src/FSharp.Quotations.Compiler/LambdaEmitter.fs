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

open System
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Quotations

module internal LambdaEmitter =
  let private emitCtor (baseType: Type) (lambda: TypeBuilderWrapper) (fields: FieldBuilder list) (varEnv: VariableEnv) =
    let varNamesAndTypes = varEnv |> List.map (fun (v, _) -> (v.Name, v.Type))
    let baseCtor = baseType.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [||], null)
    let ctor = lambda.DefineConstructor(MethodAttributes.Public, varNamesAndTypes)
    let ctorGen = ctor.GetILGenerator(varNamesAndTypes)
    try
      // emit: call base ctor
      ctorGen.Emit(Ldarg_0)
      if varNamesAndTypes = [] then
        ctorGen.Emit(Tailcall)
      ctorGen.Emit(Call (Ctor baseCtor))

      for i, field in List.zip [1..fields.Length] fields do
        // emit: this.field <- arg_i
        ctorGen.Emit(Ldarg_0)
        ctorGen.Emit(Ldarg i)
        ctorGen.Emit(Stfld field)
      ctorGen.Emit(Ret)
      ctor.RawBuilder :> ConstructorInfo
    finally
      ctorGen.Close()

  let private emitInvoke (baseType: Type) (lambda: TypeBuilderWrapper, fields, ctor) (gen, varEnvRef: VariableEnv ref, needVarInfos: VariableEnv, argVar: Var, bodyType: Type) bodyCompileInfo (stack: CompileStack) =
    let varEnv = !varEnvRef

    let invoke = lambda.DefineOverrideMethod(baseType, "Invoke", MethodAttributes.Public, bodyType, [ argVar ])
    let invokeGen = invoke.GetILGenerator(argVar, bodyType)
    stack.Push(Compiling (fun _ ->
      varEnvRef := varEnv
    ))
    stack.Push(Compiling (fun gen ->
      for _, info in needVarInfos do
        match info with
        | Arg i -> gen.Emit(Ldarg i)
        | Local (local, name) -> gen.Emit(ILOpCode.ldloc local name)
        | Field fi -> gen.Emit(Ldarg_0); gen.Emit(Ldfld fi)
      gen.Emit(Newobj ctor)
    ))
    stack.Push(RestoreGen gen)

    stack.Push(Compiling (fun gen ->
      gen.Emit(Ret)
      lambda.CreateType() |> ignore
    ))
    stack.Push(Assumption IfRet)
    stack.Push(bodyCompileInfo)
    let newVarEnv = List.zip needVarInfos fields |> List.map (fun ((var, _), fi) -> (var, Field fi))
    stack.Push(Compiling (fun _ ->
      varEnvRef := (argVar, Arg 1)::newVarEnv
    ))
    
    invokeGen

  let emit (parentMod: ModuleBuilderWrapper) (gen, varEnvRef: VariableEnv ref, argVar: Var, bodyType: Type) bodyCompileInfo (stack: CompileStack) =
    let varEnv = !varEnvRef

    let lambda = parentMod.DefineLambda(argVar.Type, bodyType)
    let baseType = lambda.BaseType

    let needVarInfos =
      varEnv
      |> List.fold (fun acc (v, info) -> if List.forall (fun (v2, _) -> v <> v2) acc then (v, info)::acc else acc) []
      |> List.rev

    let fields = needVarInfos |> List.map (fun (var, _) -> lambda.DefineField(var.Name, var.Type, FieldAttributes.Private))
    let ctor = emitCtor baseType lambda fields needVarInfos
    emitInvoke baseType (lambda, fields, ctor) (gen, varEnvRef, needVarInfos, argVar, bodyType) bodyCompileInfo stack
