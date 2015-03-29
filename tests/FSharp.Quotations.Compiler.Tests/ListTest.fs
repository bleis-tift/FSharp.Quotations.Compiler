namespace FSharp.Quotations.Compiler.Tests

#nowarn "1104"

[<TestModule>]
module ListTest =
  [<Test>]
  let ``int list @ int list`` () =
    <@ [1; 2; 3] @ [4; 5; 6] @> |> check [1..6]
    <@ [1; 2; 3] @ [] @> |> check [1..3]
    <@ [] @ [4; 5; 6] @> |> check [4..6]

  [<Test>]
  let ``string list @ string list`` () =
    <@ ["a"; "b"] @ ["c"; "d"] @> |> check ["a"; "b"; "c"; "d"]
    <@ ["a"; "b"] @ [] @> |> check ["a"; "b"]
    <@ [] @ ["c"; "d"] @> |> check ["c"; "d"]

  [<Test>]
  let ``cons`` () =
    <@ 1::2::3::[] @> |> check [1..3]
    <@ "a"::"b"::"c"::[] @> |> check ["a"; "b"; "c"]

  [<Test>]
  let ``cons match`` () =
    <@ match [1..3] with
       | x::xs -> (x * 10) + (xs.Length)
       | [] -> -1 @>
    |> check 12

  [<Test>]
  let ``empty`` () =
    <@ List.empty : int list @> |> check []
    <@ List.empty : string list @> |> check []

  [<Test>]
  let ``sum`` () = <@ List.sum [1..10] @> |> check (List.sum [1..10])

  [<Test>]
  let ``sumBy`` () = <@ List.sumBy int ["1"; "2"] @> |> check 3

  [<Test>]
  let ``filter`` () =
    <@ [1..10] |> List.filter (fun x -> x < 3) @> |> check [1; 2]
    <@ [1..10] |> List.filter ((=)5) @> |> check [5]