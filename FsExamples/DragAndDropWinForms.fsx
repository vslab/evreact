#r @"..\lib\evReact.dll"

open EvReact
open EvReact.Expr
open EvReact.Orchestrator

open System.Windows.Forms

let E (e:IEvent<'c,'a>) = 
  let evt = new Control.Event<'a>()
  e.Add(fun e -> evt.Trigger(e))
  evt.Publish

let f = new Form(Text="Drag&Drop test with evReact")

let md = E f.MouseDown
let mm = E f.MouseMove
let mu = E f.MouseUp

let net = 
  +(
      (!!md |-> fun e -> printfn "Mouse down @(%d,%d)" e.X e.Y) 
    - (+(!!mm) |-> fun e -> printfn "Mouse move @(%d,%d)" e.X e.Y) / [|mu|]
    - !!mu |-> fun e -> printfn "Mouse up @(%d,%d)" e.X e.Y
  )

let orch = Orchestrator.create()
Expr.start null orch net

f.Show()
