namespace EvReact.Test

open NUnit.Framework
open EvReact
open EvReact.Expr
open EvReact.Test.Utils

[<TestFixture>]
type SerializeTest() =    
  [<Test>]
  member this.BinaryAll () =
    let success = ref false
    let e = Event.create "e"
    let i = e.Publish
    let orch = Orchestrator.createDebug()
    let expr = !!i - !!i |-> (fun _ -> success := true)
    let net = start () orch expr
    e.Trigger()

    let newsuccess,newe,neworch,newnet = serializationClone (success,e,orch,net)

    Assert.IsFalse(orch.IsEmpty)
    stop net
    Assert.IsTrue(orch.IsEmpty)
    Assert.IsFalse(!success)

    Assert.IsFalse(!newsuccess)
    Assert.IsFalse(neworch.IsEmpty)
    newe.Trigger()
    Assert.IsTrue(!newsuccess)
    Assert.IsTrue(neworch.IsEmpty)
    
  [<Test>]
  member this.BinaryEvent () =
    let success = ref false
    let e = Event.create "e"
    let i = e.Publish
    let orch = Orchestrator.create()
    let expr = !!i - !!i |-> (fun _ -> success := true)
    let net = start () orch expr
    e.Trigger()
    let newsuccess,newe = serializationClone (success,e)
    stop net
    Assert.IsFalse(!success)
    Assert.IsFalse(!newsuccess)
    newe.Trigger()
    Assert.IsTrue(!newsuccess)
