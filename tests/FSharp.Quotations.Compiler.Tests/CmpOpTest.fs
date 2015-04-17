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
module CmpOpTest =
  [<Test>]
  let ``int = int`` () =
    <@ 1 = 1 @> |> check true
    <@ 0 = 1 @> |> check false

  [<Test>]
  let ``int <> int`` () =
    <@ 1 <> 1 @> |> check false
    <@ 0 <> 1 @> |> check true

  [<Test>]
  let ``int < int`` () =
    <@ 1 < 0 @> |> check false
    <@ 1 < 1 @> |> check false
    <@ 0 < 1 @> |> check true

  [<Test>]
  let ``int <= int`` () =
    <@ 1 <= 0 @> |> check false
    <@ 1 <= 1 @> |> check true
    <@ 0 <= 1 @> |> check true

  [<Test>]
  let ``int > int`` () =
    <@ 1 > 0 @> |> check true
    <@ 1 > 1 @> |> check false
    <@ 0 > 1 @> |> check false

  [<Test>]
  let ``int >= int`` () =
    <@ 1 >= 0 @> |> check true
    <@ 1 >= 1 @> |> check true
    <@ 0 >= 1 @> |> check false

  [<Test>]
  let ``bigint = bigint`` () =
    <@ 1I = 1I @> |> check true
    <@ 1I = 0I @> |> check false

  [<Test>]
  let ``bigint <> bigint`` () =
    <@ 1I = 1I @> |> check true
    <@ 1I = 0I @> |> check false

  [<Test>]
  let ``bigint < bigint`` () =
    <@ 1I < 0I @> |> check false
    <@ 1I < 1I @> |> check false
    <@ 0I < 1I @> |> check true

  [<Test>]
  let ``bigint <= bigint`` () =
    <@ 1I <= 0I @> |> check false
    <@ 1I <= 1I @> |> check true
    <@ 0I <= 1I @> |> check true

  [<Test>]
  let ``bigint > bigint`` () =
    <@ 1I > 0I @> |> check true
    <@ 1I > 1I @> |> check false
    <@ 0I > 1I @> |> check false

  [<Test>]
  let ``bigint >= bigint`` () =
    <@ 1I >= 0I @> |> check true
    <@ 1I >= 1I @> |> check true
    <@ 0I >= 1I @> |> check false

  let ``char = char`` () =
    <@ 'a' = 'a' @> |> check true
    <@ 'a' = 'b' @> |> check false

  [<Test>]
  let ``char <> char`` () =
    <@ 'a' <> 'a' @> |> check false
    <@ 'a' <> 'b' @> |> check true

  [<Test>]
  let ``char < char`` () =
    <@ 'a' < 'a' @> |> check false
    <@ 'a' < 'b' @> |> check true
    <@ 'b' < 'a' @> |> check false

  [<Test>]
  let ``char <= char`` () =
    <@ 'a' <= 'a' @> |> check true
    <@ 'a' <= 'b' @> |> check true
    <@ 'b' <= 'a' @> |> check false

  [<Test>]
  let ``char > char`` () =
    <@ 'a' > 'a' @> |> check false
    <@ 'a' > 'b' @> |> check false
    <@ 'b' > 'a' @> |> check true

  [<Test>]
  let ``char >= char`` () =
    <@ 'a' >= 'a' @> |> check true
    <@ 'a' >= 'b' @> |> check false
    <@ 'b' >= 'a' @> |> check true

  [<Test>]
  let ``float = float`` () =
    <@ 1.0 = 1.0 @> |> check true
    <@ 0.0 = 1.0 @> |> check false

  [<Test>]
  let ``float <> float`` () =
    <@ 1.0 <> 1.0 @> |> check false
    <@ 0.0 <> 1.0 @> |> check true

  [<Test>]
  let ``float < float`` () =
    <@ 1.0 < 0.0 @> |> check false
    <@ 1.0 < 1.0 @> |> check false
    <@ 0.0 < 1.0 @> |> check true

  [<Test>]
  let ``float <= float`` () =
    <@ 1.0 <= 0.0 @> |> check false
    <@ 1.0 <= 1.0 @> |> check true
    <@ 0.0 <= 1.0 @> |> check true

  [<Test>]
  let ``float > float`` () =
    <@ 1.0 > 0.0 @> |> check true
    <@ 1.0 > 1.0 @> |> check false
    <@ 0.0 > 1.0 @> |> check false

  [<Test>]
  let ``float >= float`` () =
    <@ 1.0 >= 0.0 @> |> check true
    <@ 1.0 >= 1.0 @> |> check true
    <@ 0.0 >= 1.0 @> |> check false

  [<Test>]
  let ``decimal = decimal`` () =
    <@ 1M = 1M @> |> check true
    <@ 1M = 0M @> |> check false

  [<Test>]
  let ``decimal <> decimal`` () =
    <@ 1M = 1M @> |> check true
    <@ 1M = 0M @> |> check false

  [<Test>]
  let ``decimal < decimal`` () =
    <@ 1M < 0M @> |> check false
    <@ 1M < 1M @> |> check false
    <@ 0M < 1M @> |> check true

  [<Test>]
  let ``decimal <= decimal`` () =
    <@ 1M <= 0M @> |> check false
    <@ 1M <= 1M @> |> check true
    <@ 0M <= 1M @> |> check true

  [<Test>]
  let ``decimal > decimal`` () =
    <@ 1M > 0M @> |> check true
    <@ 1M > 1M @> |> check false
    <@ 0M > 1M @> |> check false

  [<Test>]
  let ``decimal >= decimal`` () =
    <@ 1M >= 0M @> |> check true
    <@ 1M >= 1M @> |> check true
    <@ 0M >= 1M @> |> check false

  [<Test>]
  let ``bool = bool`` () =
    <@ true = true @> |> check true
    <@ false = true @> |> check false

  [<Test>]
  let ``bool <> bool`` () =
    <@ true <> true @> |> check false
    <@ false <> true @> |> check true

  [<Test>]
  let ``bool < bool`` () =
    <@ true < false @> |> check false
    <@ true < true @> |> check false
    <@ false < true @> |> check true

  [<Test>]
  let ``bool <= bool`` () =
    <@ true <= false @> |> check false
    <@ true <= true @> |> check true
    <@ false <= true @> |> check true

  [<Test>]
  let ``bool > bool`` () =
    <@ true > false @> |> check true
    <@ true > true @> |> check false
    <@ false > true @> |> check false

  [<Test>]
  let ``bool >= bool`` () =
    <@ true >= false @> |> check true
    <@ true >= true @> |> check true
    <@ false >= true @> |> check false

  [<Test>]
  let ``string = string`` () =
    <@ "aaa" = "aaa" @> |> check true
    <@ "bbb" = "aaa" @> |> check false

  [<Test>]
  let ``string <> string`` () =
    <@ "aaa" <> "aaa" @> |> check false
    <@ "bbb" <> "aaa" @> |> check true

  [<Test>]
  let ``string < string`` () =
    <@ "aaa" < "bbb" @> |> check true
    <@ "aaa" < "aaa" @> |> check false
    <@ "bbb" < "aaa" @> |> check false

  [<Test>]
  let ``string <= string`` () =
    <@ "aaa" <= "bbb" @> |> check true
    <@ "aaa" <= "aaa" @> |> check true
    <@ "bbb" <= "aaa" @> |> check false

  [<Test>]
  let ``string > string`` () =
    <@ "aaa" > "bbb" @> |> check false
    <@ "aaa" > "aaa" @> |> check false
    <@ "bbb" > "aaa" @> |> check true

  [<Test>]
  let ``string >= string`` () =
    <@ "aaa" >= "bbb" @> |> check false
    <@ "aaa" >= "aaa" @> |> check true
    <@ "bbb" >= "aaa" @> |> check true

  [<Test>]
  let ``int[] = int[]`` () =
    <@ [|1|] = [|1|] @> |> check true
    <@ [|1|] = [|2|] @> |> check false
    <@ [|1|] = [|1; 1|] @> |> check false

  [<Test>]
  let ``int[] <> int[]`` () =
    <@ [|1|] <> [|1|] @> |> check false
    <@ [|1|] <> [|2|] @> |> check true
    <@ [|1|] <> [|1; 1|] @> |> check true

  [<Test>]
  let ``int[] < int[]`` () =
    <@ [|1|] < [||] @> |> check false
    <@ [|1|] < [|1|] @> |> check false
    <@ [|1|] < [|2|] @> |> check true
    <@ [|1|] < [|1; 1|] @> |> check true
    <@ [|2|] < [|1; 1|] @> |> check true

  [<Test>]
  let ``int[] <= int[]`` () =
    <@ [|1|] <= [||] @> |> check false
    <@ [|1|] <= [|1|] @> |> check true
    <@ [|1|] <= [|2|] @> |> check true
    <@ [|1|] <= [|1; 1|] @> |> check true
    <@ [|2|] <= [|1; 1|] @> |> check true

  [<Test>]
  let ``int[] > int[]`` () =
    <@ [|1|] > [||] @> |> check true
    <@ [|1|] > [|1|] @> |> check false
    <@ [|1|] > [|2|] @> |> check false
    <@ [|1|] > [|1; 1|] @> |> check false
    <@ [|2|] > [|1; 1|] @> |> check false

  [<Test>]
  let ``int[] >= int[]`` () =
    <@ [|1|] >= [||] @> |> check true
    <@ [|1|] >= [|1|] @> |> check true
    <@ [|1|] >= [|2|] @> |> check false
    <@ [|1|] >= [|1; 1|] @> |> check false
    <@ [|2|] >= [|1; 1|] @> |> check false

  [<Test>]
  let ``byte = byte`` () =
    <@ 0uy = 0uy @> |> check true
    <@ 255uy = 255uy @> |> check true
    <@ 0uy = 255uy @> |> check false
    <@ 255uy = 0uy @> |> check false

  [<Test>]
  let ``byte <> byte`` () =
    <@ 0uy <> 0uy @> |> check false
    <@ 255uy <> 255uy @> |> check false
    <@ 0uy <> 255uy @> |> check true
    <@ 255uy <> 0uy @> |> check true

  [<Test>]
  let ``byte > byte`` () =
    <@ 1uy > 0uy @> |> check true
    <@ 255uy > 1uy @> |> check true
    <@ 255uy > 254uy @> |> check true
    <@ 0uy > 255uy @> |> check false
    <@ 255uy > 255uy @> |> check false

  [<Test>]
  let ``byte >= byte`` () =
    <@ 1uy >= 0uy @> |> check true
    <@ 255uy >= 1uy @> |> check true
    <@ 255uy >= 254uy @> |> check true
    <@ 255uy >= 255uy @> |> check true
    <@ 0uy >= 255uy @> |> check false
    <@ 254uy >= 255uy @> |> check false

  [<Test>]
  let ``byte < byte`` () =
    <@ 0uy < 1uy @> |> check true
    <@ 1uy < 255uy @> |> check true
    <@ 254uy < 255uy @> |> check true
    <@ 255uy < 0uy @> |> check false
    <@ 255uy < 255uy @> |> check false

  [<Test>]
  let ``byte <= byte`` () =
    <@ 0uy <= 1uy @> |> check true
    <@ 1uy <= 255uy @> |> check true
    <@ 254uy <= 255uy @> |> check true
    <@ 255uy <= 255uy @> |> check true
    <@ 255uy <= 0uy @> |> check false
    <@ 255uy <= 254uy @> |> check false
