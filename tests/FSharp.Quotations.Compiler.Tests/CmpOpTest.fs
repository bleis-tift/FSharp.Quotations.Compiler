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
