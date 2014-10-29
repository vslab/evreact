namespace EvReact.Test

open NUnit.Framework
open EvReact
open EvReact.Expr
open EvReact.Test.Utils

[<TestFixture>]
type SimpleAATest() =
  [<Test>]
  member this.SimpleCatAA () =
    let e,check = prepare [ "aa!"; "bbbabbba!bbb" ]
    !!(e 'a') - !!(e 'a')
    |> check

  [<Test>]
  member this.SimpleAndAA () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    !!(e 'a') &&& !!(e 'a')
    |> check

  [<Test>]
  member this.SimpleOrAA () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    !!(e 'a') ||| !!(e 'a')
    |> check
