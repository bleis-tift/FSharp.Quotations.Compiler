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
module BitOpTest =
  [<Test>]
  let ``int &&& int`` () = <@ 42 &&& 63 @> |> check 42

  [<Test>]
  let ``int ||| int`` () = <@ 42 ||| 0 @> |> check 42

  [<Test>]
  let ``int ^^^ int`` () = <@ 1 ^^^ 1 @> |> check 0

  [<Test>]
  let ``int >>> int`` () =
    <@ 42 >>> 1 @> |> check 21
    <@ 42 >>> 31 @> |> check 0
    <@ -1 >>> 1 @> |> check -1
    <@ -1 >>> 31 @> |> check -1
    <@ -1 >>> 32 @> |> check -1

  [<Test>]
  let ``int <<< int`` () =
    <@ 42 <<< 1 @> |> check 84
    <@ Int32.MaxValue <<< 1 @> |> check -2
    <@ Int32.MinValue <<< 1 @> |> check 0

  [<Test>]
  let ``~~~ int`` () = <@ ~~~1 @> |> check -2

  [<Test>]
  let ``bigint &&& bigint`` () =
    <@ 1I &&& 1I @> |> check (1I &&& 1I)

  [<Test>]
  let ``bigint ||| bigint`` () =
    <@ 1I ||| 1I @> |> check (1I ||| 1I)

  [<Test>]
  let ``bigint ^^^ bigint`` () =
    <@ 1I ^^^ 1I @> |> check (1I ^^^ 1I)

  [<Test>]
  let ``bigint >>> int`` () =
    <@ 42I >>> 1 @> |> check (42I >>> 1)

  [<Test>]
  let ``bigint <<< int`` () =
    <@ 42I <<< 1 @> |> check (42I <<< 1)
  [<Test>]
  let ``byte >>> int`` () =
    <@ 0uy >>> -1 @> |> check 0uy
    <@ 0uy >>> 0 @> |> check 0uy
    <@ 0uy >>> 1 @> |> check 0uy
    <@ 1uy >>> -2 @> |> check 0uy
    <@ 1uy >>> -1 @> |> check 0uy
    <@ 1uy >>> 0 @> |> check 1uy
    <@ 1uy >>> 1 @> |> check 0uy
    <@ 1uy >>> 2 @> |> check 0uy
    <@ 255uy >>> -256 @> |> check 255uy
    <@ 255uy >>> -255 @> |> check 127uy
    <@ 255uy >>> -2 @> |> check 3uy
    <@ 255uy >>> -1 @> |> check 1uy
    <@ 255uy >>> 0 @> |> check 255uy
    <@ 255uy >>> 1 @> |> check 127uy
    <@ 255uy >>> 2 @> |> check 63uy
    <@ 255uy >>> 255 @> |> check 1uy
    <@ 255uy >>> 256 @> |> check 255uy

  [<Test>]
  let ``byte <<< int`` () =
    <@ 0uy <<< -1 @> |> check 0uy
    <@ 0uy <<< 0 @> |> check 0uy
    <@ 0uy <<< 1 @> |> check 0uy
    <@ 1uy <<< -2 @> |> check 64uy
    <@ 1uy <<< -1 @> |> check 128uy
    <@ 1uy <<< 0 @> |> check 1uy
    <@ 1uy <<< 1 @> |> check 2uy
    <@ 1uy <<< 2 @> |> check 4uy
    <@ 255uy <<< -256 @> |> check 255uy
    <@ 255uy <<< -255 @> |> check 254uy
    <@ 255uy <<< -254 @> |> check 252uy
    <@ 255uy <<< -2 @> |> check 192uy
    <@ 255uy <<< -1 @> |> check 128uy
    <@ 255uy <<< 0 @> |> check 255uy
    <@ 255uy <<< 1 @> |> check 254uy
    <@ 255uy <<< 2 @> |> check 252uy
    <@ 255uy <<< 255 @> |> check 128uy
    <@ 255uy <<< 256 @> |> check 255uy

  [<Test>]
  let ``byte ^^^ byte`` () =
    <@ 0uy ^^^ 0uy @> |> check 0uy
    <@ 0uy ^^^ 1uy @> |> check 1uy
    <@ 1uy ^^^ 0uy @> |> check 1uy
    <@ 1uy ^^^ 1uy @> |> check 0uy
    <@ 1uy ^^^ 2uy @> |> check 3uy
    <@ 255uy ^^^ 0uy @> |> check 255uy
    <@ 255uy ^^^ 1uy @> |> check 254uy
    <@ 255uy ^^^ 128uy @> |> check 127uy
    <@ 255uy ^^^ 254uy @> |> check 1uy
    <@ 255uy ^^^ 255uy @> |> check 0uy

  [<Test>]
  let ``byte ||| byte`` () =
    <@ 0uy ||| 0uy @> |> check 0uy
    <@ 0uy ||| 1uy @> |> check 1uy
    <@ 0uy ||| 255uy @> |> check 255uy
    <@ 1uy ||| 0uy @> |> check 1uy
    <@ 1uy ||| 1uy @> |> check 1uy
    <@ 1uy ||| 255uy @> |> check 255uy
    <@ 128uy ||| 0uy @> |> check 128uy
    <@ 128uy ||| 1uy @> |> check 129uy
    <@ 128uy ||| 126uy @> |> check 254uy
    <@ 128uy ||| 127uy @> |> check 255uy
    <@ 128uy ||| 255uy @> |> check 255uy
    <@ 255uy ||| 0uy @> |> check 255uy
    <@ 255uy ||| 1uy @> |> check 255uy
    <@ 255uy ||| 128uy @> |> check 255uy
    <@ 255uy ||| 254uy @> |> check 255uy
    <@ 255uy ||| 255uy @> |> check 255uy

  [<Test>]
  let ``byte &&& byte`` () =
    <@ 0uy &&& 0uy @> |> check 0uy
    <@ 0uy &&& 1uy @> |> check 0uy
    <@ 0uy &&& 255uy @> |> check 0uy
    <@ 1uy &&& 0uy @> |> check 0uy
    <@ 1uy &&& 1uy @> |> check 1uy
    <@ 1uy &&& 255uy @> |> check 1uy
    <@ 128uy &&& 0uy @> |> check 0uy
    <@ 128uy &&& 127uy @> |> check 0uy
    <@ 128uy &&& 128uy @> |> check 128uy
    <@ 128uy &&& 255uy @> |> check 128uy
    <@ 255uy &&& 0uy @> |> check 0uy
    <@ 255uy &&& 1uy @> |> check 1uy
    <@ 255uy &&& 128uy @> |> check 128uy
    <@ 255uy &&& 254uy @> |> check 254uy
    <@ 255uy &&& 255uy @> |> check 255uy

  [<Test>]
  let ``~~~ byte`` () =
    <@ ~~~ 0uy @> |> check 255uy
    <@ ~~~ 1uy @> |> check 254uy
    <@ ~~~ 254uy @> |> check 1uy
    <@ ~~~ 255uy @> |> check 0uy
