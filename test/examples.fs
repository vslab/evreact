namespace EvReact.Test

open NUnit.Framework
open EvReact
open EvReact.Expr
open EvReact.Test.Utils

[<TestFixture>]
type ExampleTests() =
  [<Test>]
  member this.Balanced () =
    let nets = new System.Collections.Generic.List<System.IDisposable>()

    let makeGrammar expr suffix =
      let m = Event.create suffix
      (epsilon |-> (expr m.Trigger >> ignore)) &&& iter !!m.Publish

    let balancedParens orch ``(``  ``)`` callback =
      let bound = [| ``(``; ``)`` |]
      let ``(`` = !! ``(`` / bound
      let ``)`` = !! ``)`` / bound
      let rec expr cb () =
        let abort = Event.create "abort"
        let expr = makeGrammar expr "E" / [| abort.Publish |]
        let expr =
          epsilon |||
          +(``(`` - expr - ``)`` |-> abort.Trigger)
        let expr = expr |-> cb
        let net = start () orch expr
        nets.Add(net)
        net
      expr callback ()

    let orch = Orchestrator.createDebug()
    let o = Event.create "-(-"
    let c = Event.create "-)-"
    let count = ref 0
    Assert.IsTrue(orch.IsEmpty)
    let net = balancedParens orch o.Publish c.Publish (fun () -> Assert.AreEqual(0, !count))
    Assert.IsFalse(orch.IsEmpty)
    for v in "(((())))(()(()))" do
      match v with
        | '(' -> count := !count + 1 ; o.Trigger()
        | ')' -> count := !count - 1 ; c.Trigger()
        | _ -> failwith "Error"
    Assert.IsFalse(orch.IsEmpty)
    stop net
    Assert.IsTrue(orch.IsEmpty)

  [<Test>]
  member this.PingPong () =
    let orch = Orchestrator.createDebug()
    let a = Event.create "a"
    let b = Event.create "b"
    let count = ref 0
    Assert.IsTrue(orch.IsEmpty)
    let net (x:Event<_>) (y:Event<_>) =
      let post = x.Trigger
      let receive = !!y.Publish
      +receive |-> fun v -> if v > 0 then post (v-1)
    let net1 = start 0 orch <| net a b
    let net2 = start 0 orch <| net b a
    Assert.IsFalse(orch.IsEmpty)
    a.Trigger(10)
    Assert.IsFalse(orch.IsEmpty)
    stop net1
    stop net2
    Assert.IsTrue(orch.IsEmpty)

  [<Test>]
  member this.If () =
    let ifElseExpr cond trueExpr falseExpr =
      let e = Event.create "e"
      let ie = e.Publish
      let restriction = [| ie |]
      let trueBranch = (ie %- cond) / restriction - trueExpr
      let falseBranch = (ie %- (cond >> not)) / restriction - falseExpr
      (epsilon |-> e.Trigger) - (trueBranch ||| falseBranch)

    let em = Event.create "m"
    let im = em.Publish
    let n = ref 0
    let guard _ = 0 = !n % 3
    let incrN _ = incr n

    let e,check = prepare [ "aaa."; "bbbabbbabbba.bbbabbbabbba.bbb" ]
    +((!!(e 'a') |-> incrN) - ifElseExpr guard (epsilon |-> em.Trigger) epsilon) - !!im
    |> check

  [<Test>]
  member this.RetryAlways () =
    let retryAlways expr =
      let e = Event.create "e"
      let ie = e.Publish
      +(((expr |=> e.Trigger) ||| epsilon) - !!ie)

    let e,check = prepare [ "bc.a"; "cccbbbc.a.ccca.bbba.ba.cbc." ]
    retryAlways ((!!(e 'b') - !!(e 'c')) / [| e 'a' |])
    |> check

  [<Test>]
  member this.Shortcut () =
    let shortcut expr =
      let e = Event.create "e"
      let ie = e.Publish
      (expr |-> e.Trigger) / [| ie |]

    let e,check = prepare [ "b!"; "a!"; "cccb!cccaaa"; "ccca!aaacccbbb" ]
    shortcut (!!(e 'a') ||| !!(e 'b'))
    |> check

  [<Test>]
  member this.SoftNegation () =
    let negation expr =
      let e = Event.create "e"
      let ie = e.Publish
      !!ie ||| (never &&& expr |=> e.Trigger)
    let e,check = prepare [ "b!"; "aaacccaaab!" ]
    negation +(!!(e 'a') / [| e 'b' |])
    |> check

  [<Test>]
  member this.HardNegation () =
    let negation expr =
      let fail = Event.create "fail"
      let ifail = fail.Publish
      let success = Event.create "success"
      let isuccess = success.Publish
      (((expr |-> fail.Trigger |=> success.Trigger) - never) ||| !!isuccess) / [| ifail |]
    let e,check = prepare [ "b!"; "a0aacccaaab" ]
    negation +(!!(e 'a') / [| e 'b' |])
    |> check
