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

[<TestModule>]
module TypeOpTest =

  [<Test>]
  let ``int :> obj`` () = <@ 42 :> obj @> |> check (42 :> obj)

  [<Test>]
  let ``bigint :> obj`` () = <@ 1I :> obj @> |> check (1I :> obj)

  [<Test>]
  let `` char :> obj`` () = <@ 'c' :> obj @> |> check ('c' :> obj)

  [<Test>]
  let ``decimal :> obj`` () = <@ 1M :> obj @> |> check (1M :> obj)

  [<TestCase(0uy)>]
  [<TestCase(1uy)>]
  [<TestCase(254uy)>]
  [<TestCase(255uy)>]
  let ``byte :> obj`` (b:byte) = <@ b :> obj @> |> check (b :> obj)

  [<Test>]
  let ``string :> obj`` () = <@ "hoge" :> obj @> |> check ("hoge" :> obj)

  [<Test>]
  let ``int :> IEquatable<int>`` () =
    <@ 42 :> IEquatable<int> @> |> check (42 :> IEquatable<int>)

  [<Test>]
  let ``bigint :> IEquatable<bigint>`` () =
    <@ 42I :> IEquatable<bigint> @> |> check (42I :> IEquatable<bigint>)

  [<Test>]
  let ``char :> IEquatable<char>`` () =
    <@ 'c' :> IEquatable<char> @> |> check ('c' :> IEquatable<char>)

  [<Test>]
  let ``decimal :> IEquatable<decimal>`` () =
    <@ 1M :> IEquatable<decimal> @> |> check (1M :> IEquatable<decimal>)

  [<TestCase(0uy)>]
  [<TestCase(1uy)>]
  [<TestCase(254uy)>]
  [<TestCase(255uy)>]
  let ``byte :> IEquatable<decimal>`` (b:byte) =
    <@ b :> IEquatable<byte> @> |> check (b :> IEquatable<byte>)

  [<Test>]
  let ``string :> IEquatable<string>`` () =
    <@ "hoge" :> IEquatable<string> @> |> check ("hoge" :> IEquatable<string>)
