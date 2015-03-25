namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module CoreFuncTest =
  [<Test>]
  let ``abs int`` () = <@ abs -1 @> |> check 1

  [<Test>]
  let ``box int`` () = <@ box 1 @> |> check (box 1)

  [<Test>]
  let ``box bool`` () = <@ box true @> |> check (box true)

  [<Test>]
  let ``box string`` () = <@ box "str" @> |> check (box "str")

  [<Test>]
  let ``failwith string`` () = <@ failwith "str" @> |> checkExnType typeof<exn>

  [<Test>]
  let ``compare int int`` () = <@ compare 1 1 @> |> check 0

  [<Test>]
  let ``compare bool bool`` () = <@ compare true true @> |> check 0

  [<Test>]
  let ``compare string string`` () = <@ compare "str" "str" @> |> check 0

  [<Test>]
  let ``defaultArg (string option) string`` () =
    <@ defaultArg None "default" @> |> check "default"
    <@ defaultArg (Some "str") "default" @> |> check "str"

  [<Test>]
  let ``defaultArg with pipeline op`` () =
    <@ (None, "default") ||> defaultArg @> |> check "default"
    <@ (Some "str", "default") ||> defaultArg @> |> check "str"

  [<Test>]
  let ``hash int`` () = <@ hash 1 @> |> check 1

  [<Test>]
  let ``hash bool`` () = <@ hash true @> |> check 1

  [<Test>]
  let ``hash string`` () = <@ hash "str" @> |> check (hash "str")

  [<Test>]
  let ``limitedHash int int`` () = <@ limitedHash 1 1 @> |> check 1

  [<Test>]
  let ``limitedHash int bool`` () = <@ limitedHash 1 true @> |> check 1

  [<Test>]
  let ``limitedHash int string`` () = <@ limitedHash 1 "str" @> |> check (limitedHash 1 "str")

  [<Test>]
  let ``id int`` () = <@ id 1 @> |> check 1

  [<Test>]
  let ``id bool`` () = <@ id true @> |> check true

  [<Test>]
  let ``id string`` () = <@ id "str" @> |> check "str"

  [<Test>]
  let ``ignore int`` () = <@ ignore 1 @> |> check ()

  [<Test>]
  let ``ignore bool`` () = <@ ignore true @> |> check ()

  [<Test>]
  let ``ignore string`` () = <@ ignore "str" @> |> check ()

  [<Test>]
  let ``invalidArg string string`` () =
    <@ invalidArg "a" "b" @> |> checkExnType typeof<System.ArgumentException>

  [<Test>]
  let ``invalidOp string`` () =
    <@ invalidOp "a" @> |> checkExnType typeof<System.InvalidOperationException>

  [<Test>]
  let ``max int int`` () = <@ max 10 20 @> |> check 20

  [<Test>]
  let ``max bool bool`` () = <@ max false true @> |> check true

  [<Test>]
  let ``max string string`` () = <@ max "aaa" "bbb" @> |> check "bbb"

  [<Test>]
  let ``min int int`` () = <@ min 10 20 @> |> check 10

  [<Test>]
  let ``min bool bool`` () = <@ min false true @> |> check false

  [<Test>]
  let ``min string string`` () = <@ min "aaa" "bbb" @> |> check "aaa"

  [<Test>]
  let ``not bool`` () = <@ not true @> |> check false

  [<Test>]
  let ``nullArg string`` () =
    <@ nullArg "a" @> |> checkExnType typeof<System.ArgumentNullException>

  [<Test>]
  let ``sign int`` () = <@ sign -10 @> |> check -1

  [<Test>]
  let ``fst int * string`` () = <@ fst (1, "str") @> |> check 1

  [<Test>]
  let ``snd int * string`` () = <@ snd (1, "str") @> |> check "str"

  module Unchecked =
    open Microsoft.FSharp.Core.Operators.Unchecked

    [<Test>]
    let ``Unchecked.compre int int`` () = <@ compare 1 1 @> |> check 0

    [<Test>]
    let ``Unchecked.hash int`` () = <@ hash 1 @> |> check 1

    [<Test>]
    let ``Unchecked.equals int int`` () = <@ equals 1 1 @> |> check true