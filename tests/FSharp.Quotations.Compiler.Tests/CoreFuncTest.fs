namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module CoreFuncTest =
  [<Test>]
  let ``box int`` () = <@ box 1 @> |> check (box 1)

  [<Test>]
  let ``box char`` () = <@ box 'a' @> |> check (box 'a')

  [<Test>]
  let ``box float`` () = <@ box 1.0 @> |> check (box 1.0)

  [<Test>]
  let ``box bool`` () = <@ box true @> |> check (box true)

  [<Test>]
  let ``box string`` () = <@ box "str" @> |> check (box "str")

  [<Test>]
  let ``box int[]`` () = <@ box [|10|] @> |> check (box [|10|])

  [<Test>]
  let ``failwith string`` () = <@ failwith "str" @> |> checkExnType typeof<exn>

  [<Test>]
  let ``compare int int`` () = <@ compare 1 1 @> |> check 0

  [<Test>]
  let ``compare char char`` () = <@ compare 'a' 'a' @> |> check 0

  [<Test>]
  let ``compare float float`` () = <@ compare 1.0 1.0 @> |> check 0

  [<Test>]
  let ``compare bool bool`` () = <@ compare true true @> |> check 0

  [<Test>]
  let ``compare string string`` () = <@ compare "str" "str" @> |> check 0

  [<Test>]
  let ``compare int[] int[]`` () = <@ compare [|10|] [|10|] @> |> check 0

  [<Test>]
  let ``defaultArg (string option) string`` () =
    <@ defaultArg None "default" @> |> check "default"
    <@ defaultArg (Some "str") "default" @> |> check "str"

  [<Test>]
  let ``defaultArg (char option) char `` () =
    <@ defaultArg None 'd' @> |> check 'd'
    <@ defaultArg (Some 'c') 'd' @> |> check 'c'

  [<Test>]
  let ``defaultArg with pipeline op`` () =
    <@ (None, "default") ||> defaultArg @> |> check "default"
    <@ (Some "str", "default") ||> defaultArg @> |> check "str"

  [<Test>]
  let ``hash int`` () = <@ hash 1 @> |> check 1

  [<Test>]
  let ``hash char`` () = <@ hash 'c' @> |> check (hash 'c')

  [<Test>]
  let ``hash float`` () = <@ hash 1.0 @> |> check (hash 1.0)

  [<Test>]
  let ``hash bool`` () = <@ hash true @> |> check 1

  [<Test>]
  let ``hash string`` () = <@ hash "str" @> |> check (hash "str")

  [<Test>]
  let ``hash int[]`` () = <@ hash [|10|] @> |> check (hash [|10|])

  [<Test>]
  let ``limitedHash int int`` () = <@ limitedHash 1 1 @> |> check 1

  [<Test>]
  let ``limitedHash int char`` () = <@ limitedHash 1 'c' @> |> check (limitedHash 1 'c')

  [<Test>]
  let ``limitedHash int float`` () = <@ limitedHash 1 1.0 @> |> check (limitedHash 1 1.0)

  [<Test>]
  let ``limitedHash int bool`` () = <@ limitedHash 1 true @> |> check 1

  [<Test>]
  let ``limitedHash int string`` () = <@ limitedHash 1 "str" @> |> check (limitedHash 1 "str")

  [<Test>]
  let ``limitedHash int int[]`` () = <@ limitedHash 1 [|10|] @> |> check (limitedHash 1 [|10|])

  [<Test>]
  let ``id int`` () = <@ id 1 @> |> check 1

  [<Test>]
  let ``id char`` () = <@ id 'c' @> |> check 'c'

  [<Test>]
  let ``id float`` () = <@ id 1.0 @> |> check 1.0

  [<Test>]
  let ``id bool`` () = <@ id true @> |> check true

  [<Test>]
  let ``id string`` () = <@ id "str" @> |> check "str"

  [<Test>]
  let ``id int[]`` () = <@ id [|10|] @> |> check [|10|]

  [<Test>]
  let ``ignore int`` () = <@ ignore 1 @> |> check ()

  [<Test>]
  let ``ignore char`` () = <@ ignore 'c' @> |> check ()

  [<Test>]
  let ``ignore float`` () = <@ ignore 1.0 @> |> check ()

  [<Test>]
  let ``ignore bool`` () = <@ ignore true @> |> check ()

  [<Test>]
  let ``ignore string`` () = <@ ignore "str" @> |> check ()

  [<Test>]
  let ``ignore int[]`` () = <@ ignore [|10|] @> |> check ()

  [<Test>]
  let ``invalidArg string string`` () =
    <@ invalidArg "a" "b" @> |> checkExnType typeof<System.ArgumentException>

  [<Test>]
  let ``invalidOp string`` () =
    <@ invalidOp "a" @> |> checkExnType typeof<System.InvalidOperationException>

  [<Test>]
  let ``max int int`` () = <@ max 10 20 @> |> check 20

  [<Test>]
  let ``max char char`` () = <@ max 'a' 'b' @> |> check 'b'

  [<Test>]
  let ``max float float`` () = <@ max 1.0 2.0 @> |> check 2.0

  [<Test>]
  let ``max bool bool`` () = <@ max false true @> |> check true

  [<Test>]
  let ``max string string`` () = <@ max "aaa" "bbb" @> |> check "bbb"

  [<Test>]
  let ``max int[] int[]`` () = <@ max [|10|] [|20|] @> |> check [|20|]

  [<Test>]
  let ``min int int`` () = <@ min 10 20 @> |> check 10

  [<Test>]
  let ``min char char`` () = <@ min 'a' 'b' @> |> check 'a'

  [<Test>]
  let ``min float float`` () = <@ min 1.0 2.0 @> |> check 1.0

  [<Test>]
  let ``min bool bool`` () = <@ min false true @> |> check false

  [<Test>]
  let ``min string string`` () = <@ min "aaa" "bbb" @> |> check "aaa"

  [<Test>]
  let ``min int[] int[]`` () = <@ min [|10|] [|20|] @> |> check [|10|]

  [<Test>]
  let ``not bool`` () = <@ not true @> |> check false

  [<Test>]
  let ``nullArg string`` () =
    <@ nullArg "a" @> |> checkExnType typeof<System.ArgumentNullException>

  [<Test>]
  let ``fst int * string`` () = <@ fst (1, "str") @> |> check 1

  [<Test>]
  let ``snd int * string`` () = <@ snd (1, "str") @> |> check "str"

  type Disposable () =
    member val Disposed = false with get, set
    interface System.IDisposable with
      member this.Dispose () =
        this.Disposed <- true

  [<Test>]
  let ``using`` () =
    <@ let x = new Disposable()
       using x (fun _ -> ())
       x.Disposed @>
    |> check true
    <@ let x: Disposable = Unchecked.defaultof<Disposable>
       using x (fun _ -> ()) @>
    |> check ()

  module Unchecked =
    open Microsoft.FSharp.Core.Operators.Unchecked

    [<Test>]
    let ``Unchecked.compre int int`` () = <@ compare 1 1 @> |> check 0

    [<Test>]
    let ``Unchecked.compare char char`` () = <@ compare 'a' 'a' @> |> check 0

    [<Test>]
    let ``Unchecked.compare float float`` () = <@ compare 1.0 1.0 @> |> check 0

    [<Test>]
    let ``Unchecked.hash int`` () = <@ hash 1 @> |> check 1

    [<Test>]
    let ``Unchecked.hash char`` () = <@ hash 'c' @> |> check (hash 'c')

    [<Test>]
    let ``Unchecked.hash float`` () = <@ hash 1.0 @> |> check (hash 1.0)

    [<Test>]
    let ``Unchecked.equals int int`` () = <@ equals 1 1 @> |> check true

    [<Test>]
    let ``Unchecked.equals char char`` () = <@ equals 'c' 'c' @> |> check true

    [<Test>]
    let ``Unchecked.equals float flaot`` () = <@ equals 1.0 1.0 @> |> check true

    [<Test>]
    let ``Unchecked.defaultof char`` () = <@ defaultof<char> @> |> check defaultof<char>
