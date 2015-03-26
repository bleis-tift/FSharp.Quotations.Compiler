namespace FSharp.Quotations.Compiler

open System
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Quotations

module internal LambdaEmitter =

  let private genericFSharpFuncType = typedefof<_ -> _>
  let private fsharpFuncType argType retType = genericFSharpFuncType.MakeGenericType([|argType; retType|])

  let mutable private lambdaCount = 0

  let private freshLambdaName () =
    let l = lambdaCount
    lambdaCount <- lambdaCount + 1
    "lambda" + (string l)

  let private emitCtor (baseType: Type) (lambda: TypeBuilderWrapper) (fields: FieldBuilder list) (varEnv: VariableEnv) =
    let varNamesAndTypes = varEnv |> List.map (fun (n, t, _) -> (n, t))
    let baseCtor = baseType.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [||], null)
    let ctor = lambda.DefineConstructor(MethodAttributes.Public, varNamesAndTypes)
    let ctorGen = ctor.GetILGenerator()
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

  let emitInvoke (baseType: Type) (lambda: TypeBuilderWrapper, fields, ctor) (gen, varEnvRef: VariableEnv ref, needVarInfos: VariableEnv, argVar: Var, bodyType: Type) bodyCompileInfo (stack: CompileStack) =
    let varEnv = !varEnvRef

    let invoke = lambda.DefineOverrideMethod(baseType, "Invoke", MethodAttributes.Public, bodyType, [ argVar ])
    let invokeGen = invoke.GetILGenerator()
    stack.Push(Compiling (fun _ ->
      varEnvRef := varEnv
    ))
    stack.Push(Compiling (fun gen ->
      for _, _, info in needVarInfos do
        match info with
        | Arg i -> gen.Emit(Ldarg i)
        | Local (local, name) -> gen.Emit(ILOpCode.ldloc local name)
        | Field fi -> gen.Emit(Ldfld fi) // TODO : thisのload(gen.Emit(Ldarg0))は不要？
      gen.Emit(Newobj ctor)
    ))
    stack.Push(RestoreGen gen)

    stack.Push(Compiling (fun gen ->
      gen.Emit(Ret)
      lambda.CreateType() |> ignore
    ))
    stack.Push(Assumption IfRet)
    stack.Push(bodyCompileInfo)
    let newVarEnv = List.zip needVarInfos fields |> List.map (fun ((name, typ, _), fi) -> (name, typ, Field fi))
    stack.Push(Compiling (fun _ ->
      varEnvRef := (argVar.Name, argVar.Type, Arg 1)::newVarEnv
    ))
    
    invokeGen

  let emit (parentMod: ModuleBuilderWrapper) (gen, varEnvRef: VariableEnv ref, argVar: Var, bodyType: Type) bodyCompileInfo (stack: CompileStack) =
    let varEnv = !varEnvRef

    let baseType = fsharpFuncType argVar.Type bodyType
    let lambda = parentMod.DefineType(freshLambdaName (), TypeAttributes.Public, baseType, [])

    let needVarInfos =
      varEnv
      |> List.fold (fun acc (n, t, info) -> if List.forall (fun (n2, _, _) -> n <> n2) acc then (n, t, info)::acc else acc) []
      |> List.rev

    let fields = needVarInfos |> List.map (fun (name, typ, _) -> lambda.DefineField(name, typ, FieldAttributes.Private))
    let ctor = emitCtor baseType lambda fields needVarInfos
    emitInvoke baseType (lambda, fields, ctor) (gen, varEnvRef, needVarInfos, argVar, bodyType) bodyCompileInfo stack
