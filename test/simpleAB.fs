namespace EvReact.Test

open NUnit.Framework
open EvReact
open EvReact.Expr
open EvReact.Test.Utils

[<TestFixture>]
type SimpleABTest() =
  [<Test>]
  member this.SimpleCatAB () =
    let e,check = prepare [ "ab!"; "bbbab!"; "bbbcccaaacccaaacccb!aaabbbccc" ]
    !!(e 'a') - !!(e 'b')
    |> check

  [<Test>]
  member this.SimpleAndAB () =
    let e,check = prepare [ "ab!"; "ba!"; "cccaaacccaaacccb!aaabbbccc" ]
    !!(e 'a') &&& !!(e 'b')
    |> check

  [<Test>]
  member this.SimpleAndBA () =
    let e,check = prepare [ "ab!"; "ba!"; "cccaaacccaaacccb!aaabbbccc" ]
    !!(e 'b') &&& !!(e 'a')
    |> check

  [<Test>]
  member this.SimpleOrAB () =
    let e,check = prepare [ "a.b!"; "b.a!"; "ccca.aacccaaacccb!aaabbbccc" ]
    !!(e 'a') ||| !!(e 'b')
    |> check

  [<Test>]
  member this.SimpleOrBA () =
    let e,check = prepare [ "a.b!"; "b.a!"; "ccca.aacccaaacccb!aaabbbccc" ]
    !!(e 'b') ||| !!(e 'a')
    |> check
