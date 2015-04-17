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

type TypeBuilderWrapper private (moduleBuilder: ModuleBuilderWrapper, builder: TypeBuilder, name: string) =
  static member Create(moduleBuilder, builder, name) =
    TypeBuilderWrapper(moduleBuilder, builder, name)

  member __.Parent = moduleBuilder
  member __.RawBuilder = builder
  member __.Name = name
  member __.BaseType = builder.BaseType

  member __.CreateType() = builder.CreateType()

[<AutoOpen>]
module ModuleBuilderWrapperExtension =
  let private genericFSharpFuncType = typedefof<_ -> _>
  let private fsharpFuncType argType retType = genericFSharpFuncType.MakeGenericType([|argType; retType|])

  type ModuleBuilderWrapper with
    member this.DefineType(name: string, attrs: TypeAttributes, baseType: Type, interfaces: Type list) =
      let typ = this.RawBuilder.DefineType(name, attrs, baseType, Array.ofList interfaces)
      TypeBuilderWrapper.Create(this, typ, name)

    member this.DefineLambda(argType, retType) =
      this.DefineType(this.FreshTypeName("lambda"), TypeAttributes.Public, fsharpFuncType argType retType, [])