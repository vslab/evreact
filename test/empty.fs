namespace EvReact.Test

open NUnit.Framework
open EvReact
open EvReact.Expr
open EvReact.Test.Utils

[<TestFixture>]
type EmptyTest() =
  [<Test>]
  member this.EmptyCat () =
    let e,check = prepare [ "0"; "0aaa" ]
    cat [| |]
    |> check

  [<Test>]
  member this.EmptyAnd () =
    let e,check = prepare [ "!"; "!aaa" ]
    all [| |]
    |> check

  [<Test>]
  member this.EmptyOr () =
    let e,check = prepare [ "0"; "0aaa" ]
    any [| |]
    |> check
