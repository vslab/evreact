namespace EvReact.Test

open NUnit.Framework
open EvReact
open EvReact.Expr
open EvReact.Test.Utils

[<TestFixture>]
type BugsTest() =
  [<Test>]
  member this.BugIterAll () =
    let e,check = prepare [ "ab.ab.ba.ba." ; "cacacacb.cbcbcbca.cacacacb." ]
    +(!!(e 'a') &&& !!(e 'b'))
    |> check

  [<Test>]
  member this.BugAnyCatACatB () =
    let e,check = prepare [ "ab!"; "bbbab!"; "bbbcccaaacccaaacccb!aaabbbccc" ]
    any [| cat [| !!(e 'a') |] |] - (!!(e 'b'))
    |> check

  [<Test>]
  member this.BugReactACatB () =
    let triggered = Event.create "triggeredEvent"
    let e,check = prepare [ "a!"; "bbba!bbb" ]
    (!!(e 'a') |-> triggered.Trigger) - !!(triggered.Publish)
    |> check

  [<Test>]
  member this.BugIterAnyABCatC () =
    let e,check = prepare [ "ac.bc."; "abc."; "cccaaac.cccabababc." ]
    +((!!(e 'a') ||| !!(e 'b')) - !!(e 'c'))
    |> check

  [<Test>]
  member this.BugUntil () =
    let e,check = prepare [ "ab!"; "aab!"; "aaab!aaabbb" ; "aaaab!aaabbb" ]
    (+(!!(e 'a')) - !!(e 'b')) / [| e 'a'; e 'b' |]
    |> check
