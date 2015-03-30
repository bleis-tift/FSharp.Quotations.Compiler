namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module MathFuncTest =
  [<Test>]
  let ``abs int`` () = <@ abs -1 @> |> check 1

  [<Test>]
  let ``abs bigint`` () = <@ abs -1I @> |> check 1I

  [<Test>]
  let ``abs float`` () = <@ abs -1.0 @> |> check 1.0

  [<Test>]
  let ``abs decimal`` () = <@ abs -1.0M @> |> check 1.0M

  [<Test>]
  let ``acos float`` () = <@ acos 0.5 @> |> check (acos 0.5)

  [<Test>]
  let ``asin float`` () = <@ asin 0.5 @> |> check (asin 0.5)

  [<Test>]
  let ``atan float`` () = <@ atan 0.5 @> |> check (atan 0.5)

  [<Test>]
  let ``atan2 float float`` () = <@ atan2 0.5 -0.5 @> |> check (atan2 0.5 -0.5)

  [<Test>]
  let ``ceil float`` () = <@ ceil 1.5 @> |> check 2.0

  [<Test>]
  let ``ceil decimal`` () = <@ ceil 1.5M @> |> check 2.0M

  [<Test>]
  let ``cos float`` () = <@ cos 0.5 @> |> check (cos 0.5)

  [<Test>]
  let ``cosh float`` () = <@ cosh 0.5 @> |> check (cosh 0.5)

  [<Test>]
  let ``exp float`` () = <@ exp 0.5 @> |> check (exp 0.5)

  [<Test>]
  let ``floor float`` () = <@ floor 1.5 @> |> check 1.0

  [<Test>]
  let ``floor decimal`` () = <@ floor 1.5M @> |> check 1.0M

  [<Test>]
  let ``infinity`` () = <@ infinity @> |> check infinity

  [<Test>]
  let ``log float`` () = <@ log 0.5 @> |> check (log 0.5)

  [<Test>]
  let ``log10 float`` () = <@ log10 0.5 @> |> check (log10 0.5)

  [<Test>]
  let ``nan`` () = <@ nan @> |> check nan

  [<Test>]
  let ``pown bigint int`` () = <@ pown 2I 2 @> |> check (pown 2I 2)

  [<Test>]
  let ``pown float int`` () = <@ pown 0.5 2 @> |> check (pown 0.5 2)

  [<Test>]
  let ``pown decimal int`` () = <@ pown 0.5M 2 @> |> check (pown 0.5M 2)

  [<Test>]
  let ``round float`` () = <@ round 0.5 @> |> check (round 0.5)

  [<Test>]
  let ``round decimal`` () = <@ round 0.5M @> |> check (round 0.5M)

  [<Test>]
  let ``sign int`` () = <@ sign -10 @> |> check -1

  [<Test>]
  let ``sign bigint`` () = <@ sign -10I @> |> check (sign -10I)

  [<Test>]
  let ``sign float`` () = <@ sign -1.0 @> |> check -1

  [<Test>]
  let ``sign decimal`` () = <@ sign -1.0M @> |> check -1

  [<Test>]
  let ``sin float`` () = <@ sin 0.5 @> |> check (sin 0.5)

  [<Test>]
  let ``sinh float`` () = <@ sinh 0.5 @> |> check (sinh 0.5)

  [<Test>]
  let ``sqrt float`` () = <@ sqrt 0.5 @> |> check (sqrt 0.5)

  [<Test>]
  let ``tan float`` () = <@ tan 0.5 @> |> check (tan 0.5)

  [<Test>]
  let ``tanh float`` () = <@ tanh 0.5 @> |> check (tanh 0.5)

  [<Test>]
  let ``truncate float`` () = <@ truncate 0.5 @> |> check (truncate 0.5)
