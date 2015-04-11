namespace FSharp.Quotations.Compiler.Tests

open System
open Microsoft.FSharp.Linq.NullableOperators

module NumericLiteralN =
  let n x = Nullable<_>(x)

  let inline FromZero () = n LanguagePrimitives.GenericZero
  let inline FromOne () = n LanguagePrimitives.GenericOne
  let FromInt32 x = n x

[<TestModule>]
module NullableTest =
  let nil<'T when 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType> = Nullable<'T>()
  
  [<Test>]
  let ``?>=`` () =
    <@ 10N ?>= 5 @> |> check true
    <@ 5N  ?>= 5 @> |> check true
    <@ 0N  ?>= 5 @> |> check false
    <@ nil ?>= 5 @> |> check false

  [<Test>]
  let ``?>`` () =
    <@ 10N ?> 5 @> |> check true
    <@ 5N  ?> 5 @> |> check false
    <@ 0N  ?> 5 @> |> check false
    <@ nil ?> 5 @> |> check false
  
  [<Test>]
  let ``?<=`` () =
    <@ 10N ?<= 5 @> |> check false
    <@ 5N  ?<= 5 @> |> check true
    <@ 0N  ?<= 5 @> |> check true
    <@ nil ?<= 5 @> |> check false

  [<Test>]
  let ``?<`` () =
    <@ 10N ?< 5 @> |> check false
    <@ 5N  ?< 5 @> |> check false
    <@ 0N  ?< 5 @> |> check true
    <@ nil ?< 5 @> |> check false

  [<Test>]
  let ``?=`` () =
    <@ 10N ?= 5 @> |> check false
    <@ 5N  ?= 5 @> |> check true
    <@ 0N  ?= 5 @> |> check false
    <@ nil ?= 5 @> |> check false

  [<Test>]
  let ``?<>`` () =
    <@ 10N ?<> 5 @> |> check true
    <@ 5N  ?<> 5 @> |> check false
    <@ 0N  ?<> 5 @> |> check true
    <@ nil ?<> 5 @> |> check true

  [<Test>]
  let ``?+`` () =
    <@ 10N ?+ 5 @> |> check 15N
    <@ nil ?+ 5 @> |> check nil

  [<Test>]
  let ``+?`` () =
    <@ 10 +? 5N @> |> check 15N
    <@ 5 +? nil @> |> check nil

  [<Test>]
  let ``?+?`` () =
    <@ 10N ?+? 5N @> |> check 15N
    <@ 5N ?+? nil @> |> check nil
    <@ nil ?+? 5N @> |> check nil
    <@ nil ?+? nil @> |> check nil

  [<Test>]
  let ``?-`` () =
    <@ 10N ?- 5 @> |> check 5N
    <@ nil ?- 5 @> |> check nil

  [<Test>]
  let ``-?`` () =
    <@ 10 -? 5N @> |> check 5N
    <@ 5 -? nil @> |> check nil

  [<Test>]
  let ``?-?`` () =
    <@ 10N ?-? 5N @> |> check 5N
    <@ 5N ?-? nil @> |> check nil
    <@ nil ?-? 5N @> |> check nil
    <@ nil ?-? nil @> |> check nil

  [<Test>]
  let ``?*`` () =
    <@ 10N ?* 5 @> |> check 50N
    <@ nil ?* 5 @> |> check nil

  [<Test>]
  let ``*?`` () =
    <@ 10 *? 5N @> |> check 50N
    <@ 5 *? nil @> |> check nil

  [<Test>]
  let ``?*?`` () =
    <@ 10N ?*? 5N @> |> check 50N
    <@ 5N ?*? nil @> |> check nil
    <@ nil ?*? 5N @> |> check nil
    <@ nil ?*? nil @> |> check nil

  [<Test>]
  let ``?/`` () =
    <@ 10N ?/ 5 @> |> check 2N
    <@ nil ?/ 5 @> |> check nil

  [<Test>]
  let ``/?`` () =
    <@ 10 /? 5N @> |> check 2N
    <@ 5 /? nil @> |> check nil

  [<Test>]
  let ``?/?`` () =
    <@ 10N ?/? 5N @> |> check 2N
    <@ 5N ?/? nil @> |> check nil
    <@ nil ?/? 5N @> |> check nil
    <@ nil ?/? nil @> |> check nil

  [<Test>]
  let ``?%`` () =
    <@ 10N ?% 5 @> |> check 0N
    <@ nil ?% 5 @> |> check nil

  [<Test>]
  let ``%?`` () =
    <@ 10 %? 5N @> |> check 0N
    <@ 5 %? nil @> |> check nil

  [<Test>]
  let ``?%?`` () =
    <@ 10N ?%? 5N @> |> check 0N
    <@ 5N ?%? nil @> |> check nil
    <@ nil ?%? 5N @> |> check nil
    <@ nil ?%? nil @> |> check nil