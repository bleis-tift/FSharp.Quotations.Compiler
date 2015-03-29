namespace FSharp.Quotations.Compiler.Tests

module SeqTest =
  [<Test>]
  let ``int seq`` () =
    <@ seq [1..10] @> |> check (seq [1..10])
    <@ seq { 1..10 } @> |> check (seq { 1..10 })

  [<Test>]
  let ``empty`` () =
    <@ Seq.empty : int seq @> |> check Seq.empty
    <@ Seq.empty : string seq @> |> check Seq.empty

  [<Test>]
  let ``sum`` () =
    <@ Seq.sum [1..10] @> |> check (Seq.sum [1..10])
    <@ Seq.sum (seq [1..10]) @> |> check (Seq.sum (seq [1..10]))
    <@ Seq.sum (seq { 1..10 }) @> |> check (Seq.sum (seq { 1..10 }))

  [<Test>]
  let ``sumBy`` () =
    <@ Seq.sumBy int ["1"; "2"] @> |> check 3
    <@ Seq.sumBy int (seq ["1"; "2"]) @> |> check 3
    <@ Seq.sumBy int (seq { yield "1"; yield "2" }) @> |> check 3

  [<Test>]
  let ``filter`` () =
    <@ [1..10] |> Seq.filter (fun x -> x < 3) @> |> check (seq [1; 2])
    <@ [1..10] |> Seq.filter ((=)5) @> |> check (seq [5])