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

open Microsoft.FSharp.Quotations

/// Contains the type extension for <see cref="Microsoft.FSharp.Quotations.Expr{'T}"/>.
[<AutoOpen>]
module Extension =

  /// The type extension for the typed expression tree.
  type Expr<'T> with
    /// <summary>
    /// Compile the typed expression tree.
    /// </summary>
    /// <returns>
    /// The compilation result.
    /// </returns>
    member this.Compile() = ExprCompiler.compile this

    /// <summary>
    /// Compie and execute the typed expression tree.
    /// </summary>
    /// <returns>
    /// The execution result.
    /// </returns>
    member this.Execute() = this.Compile().ExecuteCompiledCode()