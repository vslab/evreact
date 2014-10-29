namespace EvReact.Test

open NUnit.Framework
open EvReact
open EvReact.Expr
open EvReact.Test.Utils

[<TestFixture>]
type SimpleTest() =
  [<Test>]
  member this.Simple () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    !!(e 'a')
    |> check

  [<Test>]
  member this.SimpleCat () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    cat [| !!(e 'a') |]
    |> check

  [<Test>]
  member this.SimpleAnd () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    all [| !!(e 'a') |]
    |> check

  [<Test>]
  member this.SimpleOr () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    any [| !!(e 'a') |]
    |> check

  [<Test>]
  member this.SimpleRestrictEmpty () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    !!(e 'a') / [| |]
    |> check

  [<Test>]
  member this.SimpleBindSame () =
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    !!(e 'a') / [| e 'a' |]
    |> check

  [<Test>]
  member this.SimpleIter () =
    let e,check = prepare [ "a.a.a."; "bbba.bbba.bbba.bbb" ]
    +(!!(e 'a'))
    |> check
