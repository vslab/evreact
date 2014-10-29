namespace EvReact.Test

open NUnit.Framework
open EvReact
open EvReact.Expr
open EvReact.Test.Utils

[<TestFixture>]
type SimpleEpsilonTest() =
  [<Test>]
  member this.SimpleCatAEpsilon () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    !!(e 'a') - epsilon
    |> check

  [<Test>]
  member this.SimpleCatEpsilonA () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    epsilon - !!(e 'a')
    |> check

  [<Test>]
  member this.SimpleAndAEpsilon () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    !!(e 'a') &&& epsilon
    |> check

  [<Test>]
  member this.SimpleAndEpsilonA () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    epsilon &&& !!(e 'a')
    |> check

  [<Test>]
  member this.SimpleOrAEpsilon () =
    let e,check = prepare [ ".a!"; ".bbba!bbb" ]
    !!(e 'a') ||| epsilon
    |> check

  [<Test>]
  member this.SimpleOrEpsilonA () =
    let e,check = prepare [ ".a!"; ".bbba!bbb" ]
    epsilon ||| !!(e 'a')
    |> check

  [<Test>]
  member this.SimpleCatEE () =
    let e,check = prepare [ "!"; "!bbb" ]
    epsilon - epsilon
    |> check

  [<Test>]
  member this.SimpleAndEE () =
    let e,check = prepare [ "!"; "!bbb" ]
    epsilon &&& epsilon
    |> check

  [<Test>]
  member this.SimpleOrEE () =
    let e,check = prepare [ "!"; "!bbb" ]
    epsilon ||| epsilon
    |> check

  [<Test>]
  member this.SimpleIterEpsilon () =
    let e,check = prepare [ "!"; "!bbb" ]
    epsilon ||| epsilon
    |> check
