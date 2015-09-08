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
namespace FSharp.Quotations.Compiler.Tests

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open FsUnit

#nowarn "25"

[<TestModule>]
module Pitfall =
  [<Test>]
  let ``pitfall: void method call expression returns unit as value of Type property`` () =
    let Call (_, mi, _) as expr = <@ Console.WriteLine() @>
    typeof<Void> |> should not' (equal typeof<unit>)
    mi.ReturnType |> should equal typeof<Void>
    expr.Type |> should equal typeof<unit>