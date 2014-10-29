namespace EvReact

  open System
  open System.Collections.Generic

  module Internals =
    let inline swap (a:List<_>) i j =
      let tmp = a.[i]
      a.[i] <- a.[j]
      a.[j] <- tmp

#if FX_NO_MONITOR_REPORTS_LOCKTAKEN

    let inline safeLock obj (taken:byref<_>) =
      System.Threading.Monitor.Enter(obj)

    let inline safeUnlock obj (taken:byref<_>) =
      System.Threading.Monitor.Exit(obj)

#else

    let inline safeLock obj (taken:byref<_>) =
      System.Threading.Monitor.Enter(obj, &taken)

    let inline safeUnlock obj (taken:byref<_>) =
      if taken then
        System.Threading.Monitor.Exit(obj)
        taken <- false

#endif

    let inline mapMax (a:_[]) f =
      let mutable r = -1
      for v in a do
        r <- max r (f v)
      r

    let inline opString (operands:_[]) opS emptyS =
      if operands.Length = 0 then
        emptyS
      else
        operands
        |> Array.map (sprintf "(%A)")
        |> String.concat opS

    (* A binary heap implementation of a Priority queue *)
    type BinaryHeap<'Key,'Value when 'Key : comparison>() =
      let keys = new List<'Key>()
      let values = new List<'Value>()

      let swap i j =
        swap keys i j
        swap values i j

      member this.Count = keys.Count

      member this.Dequeue() =
        let r = values.[0]
        let n = keys.Count - 1

        keys.[0] <- keys.[n]
        values.[0] <- values.[n]

        keys.RemoveAt(n)
        values.RemoveAt(n)

        let mutable working = n <> 0
        let mutable i = 0
        while working do
          let left = i*2
          let right = left+1
          if right < n && keys.[right] < keys.[i] && keys.[right] < keys.[left] then
            swap i right
            i <- right
          elif left < n && keys.[left] < keys.[i] then
            swap i left
            i <- left
          else
            working <- false
        r

      member this.Enqueue(key, value) =
        let mutable i = keys.Count
        keys.Add(key)
        values.Add(value)
        while i > 0 do
          let parent = i/2
          if keys.[i] < keys.[parent] then
            swap i parent
            i <- parent
          else
            i <- 0

  open Internals

  [<StructuredFormatDisplay("Event {Id}")>]
  type Event<'T>(name) =
      [<DefaultValue>] val mutable multicast : Handler<'T>
      new() = Event<'T>(null)

      member x.Id = name
      member x.Trigger(arg:'T) =
          match x.multicast with
          | null -> ()
          | d -> d.Invoke(null,arg) |> ignore
      member x.Publish =
          { new obj() with
                member e.ToString() = if x.Id = null then "<published event>" else x.Id
            interface IEvent<'T>
            interface IDelegateEvent<Handler<'T>> with
              member e.AddHandler(d) =
                  x.multicast <- (System.Delegate.Combine(x.multicast, d) :?> Handler<'T>)
              member e.RemoveHandler(d) =
                  x.multicast <- (System.Delegate.Remove(x.multicast, d) :?> Handler<'T>)
            interface System.IObservable<'T> with
              member e.Subscribe(observer) =
                 let h = new Handler<_>(fun sender args -> observer.OnNext(args))
                 (e :?> IEvent<_,_>).AddHandler(h)
                 { new System.IDisposable with
                      member x.Dispose() = (e :?> IEvent<_,_>).RemoveHandler(h) } }

  module Event =
    let create name =
      new Event<_>(name)

  type INotifiable<'T> =
    abstract member NotifyDeactivation: int * 'T -> unit
    abstract member NotifyMatch: int * 'T -> unit
    abstract member NotifyUnmatch: int * 'T -> unit


  type PrioritySet<'U,'T when 'U :> SimpleNet<'T>>() =
    let queue = new BinaryHeap<int,'U>()
    let set = new HashSet<'U>()

    member this.Count = queue.Count

    member this.Enqueue(net) =
      if set.Add(net) then
        queue.Enqueue(net.Priority, net)

    member this.Dequeue() =
      queue.Dequeue()

    member this.Reset() =
      assert (queue.Count = 0)
      set.Clear()


  and [<AbstractClass>] SimpleNet<'T>(orch, priority) =
    let mutable matching = false

    member val Parent = Unchecked.defaultof<INotifiable<'T>> with get, set
    member val Aux = 0 with get, set

    member val Orch : Orchestrator<'T> = orch
    member val Priority : int = priority

    member this.SetMatching(v, args) =
      if v then
        this.Parent.NotifyMatch(this.Aux, args)
      elif matching then
        this.Parent.NotifyUnmatch(this.Aux, args)
      matching <- v

    abstract member Start: 'T -> unit
    abstract member Stop: unit -> unit

    abstract member Initialize: unit -> unit

    interface System.IDisposable with
      member this.Dispose() = this.Stop()

  and Orchestrator<'T>() =
    let mutable evaluating = false

    let dispatchers = new Dictionary<IEvent<'T>, Dispatcher<'T>>()
    let muxers = new Dictionary<HashSet<IEvent<'T>>, Muxer<'T>>()

    let eventQueue = new Queue<IEvent<'T>>()
    let senderQueue = new Queue<obj>()
    let argsQueue = new Queue<'T>()

    let activeGroundTerms = new HashSet<GroundTermNet<'T>>()
    let activeOperators = new PrioritySet<OperatorNet<'T>,_>()
    let disablingOperators = new PrioritySet<OperatorNet<'T>,_>()
    let reactions = new PrioritySet<CallbackNet<'T>,_>()

    member this.EnqueueGroundTerm(net) =
      activeGroundTerms.Add(net) |> ignore

    member this.EnqueueOpEval(net) =
      activeOperators.Enqueue(net)

    member this.EnqueueNotifyDisable(net) =
      disablingOperators.Enqueue(net)

    member this.EnqueueReaction(net) =
      reactions.Enqueue(net)

    member this.Dispatcher(event) =
      lock dispatchers (fun () ->
        let mutable d = Unchecked.defaultof<_>
        if not (dispatchers.TryGetValue(event, &d)) then
          d <- new Dispatcher<_>(this, event)
          dispatchers.[event] <- d
        d)

    member private this.Muxer(events) =
      lock muxers (fun () ->
        let mutable m = Unchecked.defaultof<_>
        if not (muxers.TryGetValue(events, &m)) then
          m <- new Muxer<_>(this, events)
          muxers.[events] <- m
        m)

    member this.Subscribe(events) =
      new Subscription<'T>(this.Muxer(events))

    member this.Unsubscribe(event) =
      lock dispatchers (fun () -> dispatchers.Remove(event) |> ignore)

    member this.Unsubscribe(events) =
      lock muxers (fun () -> muxers.Remove(events) |> ignore)

    member this.IsEmpty =
      lock muxers (fun () ->
        lock dispatchers (fun () -> dispatchers.Count = 0 && muxers.Count = 0))

    // FIXME: this should be a virtual protected member
    member val EnqueueEvent =
      fun (orch:Orchestrator<'T>) event sender args ->
        let mutable taken = false
        let runEval =
          try
            safeLock eventQueue &taken
            let oldEvaluating = evaluating
            if evaluating then
              eventQueue.Enqueue(event)
              senderQueue.Enqueue(sender)
              argsQueue.Enqueue(args)
            else
              evaluating <- true
            not oldEvaluating
          finally
            safeUnlock eventQueue &taken

        if runEval then
          let mutable event = event
          let mutable sender = sender
          let mutable args = args
          while evaluating do
            orch.EvalEvent orch event sender args
            try
              safeLock eventQueue &taken
              evaluating <- eventQueue.Count <> 0
              if evaluating then
                event <- eventQueue.Dequeue()
                sender <- senderQueue.Dequeue()
                args <- argsQueue.Dequeue()
            finally
              safeUnlock eventQueue &taken
      with get, set

    // FIXME: this should be a virtual protected member
    member val EvalEvent =
      fun (orch:Orchestrator<'T>) event sender args ->
        if event <> Unchecked.defaultof<_> then
          lock dispatchers (fun () -> dispatchers.[event].EvalEvent(args))

        // Evaluate ground terms; unary operators are immediately
        // evaluated as soon as the subexpression [un]matches.
        for net in activeGroundTerms do
          net.Eval(args)
        activeGroundTerms.Clear()

        // Evaluate operators (bottom-up).
        while activeOperators.Count <> 0 do
          let net = activeOperators.Dequeue()
          net.SetMatching(net.Matching, args)
        activeOperators.Reset()

        while disablingOperators.Count <> 0 do
          let net = disablingOperators.Dequeue()
          if net.Active.Count = 0 then
            net.Parent.NotifyDeactivation(net.Aux, args)
        disablingOperators.Reset()

        while reactions.Count <> 0 do
          let net = reactions.Dequeue()
          net.Cb(args)
        reactions.Reset()
      with get, set


  and [<StructuredFormatDisplay("{AsString}")>] Dispatcher<'T>(orch, event) as this =
    let active = new HashSet<Muxer<'T>>()
    let inactive = new HashSet<Muxer<'T>>()
    let handler = new Handler<_>(this.HandleEvent)

    member this.AsString =
      sprintf "%A" event

    member this.HandleEvent sender args =
      orch.EnqueueEvent orch event sender args

    member this.EvalEvent(args) =
      let mutable deactivate = []
      for m in active do
        if m.EvalEvent(this, args) then
          deactivate <- m :: deactivate
      active.ExceptWith(deactivate)
      inactive.UnionWith(deactivate)
      if active.Count = 0 then
        event.RemoveHandler(handler)

    member this.Attach(mux) =
      if active.Count = 0 then
        event.AddHandler(handler)
      inactive.Remove(mux) |> ignore
      active.Add(mux) |> ignore

    member this.Detach(mux) =
      inactive.Remove(mux) |> ignore
      active.Remove(mux) |> ignore
      if active.Count = 0 then
        event.RemoveHandler(handler)
        if inactive.Count = 0 then
          orch.Unsubscribe(event)

  and [<StructuredFormatDisplay("{AsString}")>] Muxer<'T>(orch, events) =
    let activeSubscriptions = new HashSet<Subscription<'T>>()
    let inactiveSubscriptions = new HashSet<Subscription<'T>>()

    let enabledDispatchers = new HashSet<Dispatcher<'T>>()
    let disabledDispatchers = new HashSet<Dispatcher<'T>>()

    do
      for e in events do
        disabledDispatchers.Add(orch.Dispatcher(e)) |> ignore

    member this.AsString =
      if events.Count = 1 then
        events |> Seq.head |> sprintf "%A"
      else
        events |> Seq.toList |> sprintf "%A"

    member this.EvalEvent(dispatcher, args) =
      let r = activeSubscriptions.Count = 0
      if r then
        enabledDispatchers.Remove(dispatcher) |> ignore
        disabledDispatchers.Add(dispatcher) |> ignore
      else
        for s in activeSubscriptions do
          s.EvalEvent(args)
      r

    member this.Enable(subscription) =
      if activeSubscriptions.Count = 0 then
        for d in disabledDispatchers do
          d.Attach(this)
        enabledDispatchers.UnionWith(disabledDispatchers)
        disabledDispatchers.Clear()
      inactiveSubscriptions.Remove(subscription) |> ignore
      activeSubscriptions.Add(subscription) |> ignore

    member this.Disable(subscription) =
      activeSubscriptions.Remove(subscription) |> ignore
      inactiveSubscriptions.Add(subscription) |> ignore
      // disabling of dispatchers is performed lazily in EvalEvent

    member this.Unsubscribe(subscription) =
      activeSubscriptions.Remove(subscription) |> ignore
      inactiveSubscriptions.Remove(subscription) |> ignore
      if activeSubscriptions.Count = 0 && inactiveSubscriptions.Count = 0 then
        for d in enabledDispatchers do
          d.Detach(this)
        for d in disabledDispatchers do
          d.Detach(this)
        orch.Unsubscribe(events)

  and [<StructuredFormatDisplay("{AsString}")>] Subscription<'T>(mux) =
    [<DefaultValue>] val mutable EvalEvent : 'T -> unit

    member this.AsString =
      sprintf "%A" mux

    member this.Enable() =
      mux.Enable(this)

    member this.Disable() =
      mux.Disable(this)

    member this.Dispose() =
      mux.Unsubscribe(this)


  and [<AbstractClass>] UnaryOperatorNet<'T>(orch, subnet:SimpleNet<'T>) =
    inherit SimpleNet<'T>(orch, 1 + subnet.Priority)

    member val Subnet = subnet

    abstract member SideEffect : 'T -> unit
    abstract member DisablingSideEffect : 'T -> unit
    default this.DisablingSideEffect(args) = ()

    override this.Initialize() =
      this.Subnet.Parent <- this
      this.Subnet.Initialize()

    override this.Start(args) =
      this.Subnet.Start(args)

    override this.Stop() =
      this.Subnet.Stop()

    interface INotifiable<'T> with
      member this.NotifyDeactivation(aux, args) =
        this.DisablingSideEffect(args)
        this.Parent.NotifyDeactivation(this.Aux, args)

      member this.NotifyMatch(aux, args) =
        // Unary operators do not need to be queued to wait for all
        // subexpressions to complete, as they only have one.
        this.SideEffect(args)
        this.SetMatching(true, args)

      member this.NotifyUnmatch(aux, args) =
        this.SetMatching(false, args)


  and [<AbstractClass>] CallbackNet<'T>(orch, subnet, cb) =
    inherit UnaryOperatorNet<'T>(orch, subnet)

    member private this.AsString =
      sprintf "(%A) |-> ..." subnet

    member val Cb: ('T -> unit) = cb

    override this.SideEffect(args) =
      this.Orch.EnqueueReaction(this)


  and [<StructuredFormatDisplay("{AsString}")>] IterNet<'T>(orch, subnet) =
    inherit UnaryOperatorNet<'T>(orch, subnet)

    member private this.AsString =
      sprintf "+(%A)" subnet

    override this.SideEffect(args) =
      this.Start(args)


  and [<AbstractClass>] OperatorNet<'T>(orch, subnets:SimpleNet<'T>[]) =
    inherit SimpleNet<'T>(orch, 1 + mapMax subnets (fun n -> n.Priority))

    member val Subnets = subnets
    member val Active : HashSet<int> = new HashSet<_>()

    member this.SubStart(i, args) =
      this.Active.Add(i) |> ignore
      this.Subnets.[i].Start(args)

    override this.Initialize() =
      for i = 0 to this.Subnets.Length-1 do
        this.Subnets.[i].Parent <- this
        this.Subnets.[i].Aux <- i
        this.Subnets.[i].Initialize()

    override this.Stop() =
      for n in this.Subnets do
        n.Stop()

    abstract member Matching : bool
    abstract member SubMatch : int * 'T -> unit
    abstract member SubUnmatch : int * 'T -> unit

    interface INotifiable<'T> with
      member this.NotifyDeactivation(aux, args) =
        this.Active.Remove(aux) |> ignore
        if this.Active.Count = 0 then
          this.Orch.EnqueueNotifyDisable(this)

      member this.NotifyMatch(aux, args) =
        this.SubMatch(aux, args)

      member this.NotifyUnmatch(aux, args) =
        this.SubUnmatch(aux, args)


  and [<StructuredFormatDisplay("{AsString}")>] GroundTermNet<'T>(orch, predicate, e:IEvent<_>, bound:HashSet<_>) =
    inherit SimpleNet<'T>(orch, 0)

    let mutable active = false
    let mutable successful = false
    let mutable pos = Unchecked.defaultof<_>
    let mutable neg = Unchecked.defaultof<_>

    override this.Initialize() =
      pos <- orch.Subscribe(new HashSet<_>([| e |]))
      neg <- orch.Subscribe(bound)
      pos.EvalEvent <- this.PosCb
      neg.EvalEvent <- this.NegCb

    member this.Initialized = pos <> Unchecked.defaultof<_>

    member private this.AsString =
      let c = if active then "." else ""
      if bound.Count = 0 then
        sprintf "%s%A" c pos
      else
        sprintf "%s%A/%A" c pos neg

    member private this.PosCb(args) =
      if predicate args then
        successful <- true
        this.Orch.EnqueueGroundTerm(this)

    member private this.NegCb(args) =
      this.Orch.EnqueueGroundTerm(this)

    override this.Start(args) =
      successful <- false
      this.SetMatching(successful, args)
      if not active then
        pos.Enable()
        neg.Enable()
        active <- true

    member this.Eval(args:'T) : unit =
      assert active
      active <- false
      pos.Disable()
      neg.Disable()
      this.SetMatching(successful, args)
      if not active then
        this.Parent.NotifyDeactivation(this.Aux, args)

    override this.Stop() =
      if this.Initialized then
        pos.Dispose()
        neg.Dispose()
        pos <- Unchecked.defaultof<_>
        neg <- Unchecked.defaultof<_>


  type DebugOrchestrator<'T>() as this =
    inherit Orchestrator<'T>()

    let onEvent = new Event<IEvent<'T> * obj * 'T>()
    let onStepBegin = new Event<IEvent<'T> * obj * 'T>()
    let onStepEnd = new Event<IEvent<'T> * obj * 'T>()

    do
      let oldEnqueue = this.EnqueueEvent
      let oldEval = this.EvalEvent

      this.EnqueueEvent <-
        fun orch event sender args ->
          onEvent.Trigger(event, sender, args)
          oldEnqueue orch event sender args

      this.EvalEvent <-
        fun orch event sender args ->
          onStepBegin.Trigger(event, sender, args)
          oldEval orch event sender args
          onStepEnd.Trigger(event, sender, args)

    member this.IsEmpty = base.IsEmpty

    [<CLIEvent>]
    member val OnEvent = onEvent.Publish

    [<CLIEvent>]
    member val OnStepBegin = onStepBegin.Publish

    [<CLIEvent>]
    member val OnStepEnd = onStepEnd.Publish


  type [<StructuredFormatDisplay("{AsString}")>] ReactNet<'T>(orch, subnet, cb) =
    inherit CallbackNet<'T>(orch, subnet, cb)

    member private this.AsString =
      sprintf "(%A) |-> ..." subnet

    override this.SideEffect(args) =
      this.Orch.EnqueueReaction(this)


  type [<StructuredFormatDisplay("{AsString}")>] FinallyNet<'T>(orch, subnet, cb) =
    inherit CallbackNet<'T>(orch, subnet, cb)

    member private this.AsString =
      sprintf "(%A) |=> ..." subnet

    override this.DisablingSideEffect(args) =
      this.Orch.EnqueueReaction(this)

    override this.SideEffect(args) =
      ()


  [<StructuredFormatDisplay("{AsString}")>]
  type CatNet<'T>(orch, subnets) =
    inherit OperatorNet<'T>(orch, subnets)

    let mutable submatching = false

    member private this.AsString = opString this.Subnets " - " "nil"

    override this.Matching = submatching

    override this.Start(args) =
      if this.Subnets.Length <> 0 then
        submatching <- false
        this.SubStart(0, args)
      else
        this.Parent.NotifyDeactivation(this.Aux, args)

    override this.SubMatch(aux, args) =
      let next = aux + 1
      if next = this.Subnets.Length then
        submatching <- true
        this.Orch.EnqueueOpEval(this)
      else
        this.SubStart(next, args)

    override this.SubUnmatch(aux, args) =
      let next = aux + 1
      if next = this.Subnets.Length then
        submatching <- false
        this.Orch.EnqueueOpEval(this)

  [<AbstractClass>]
  type CommutativeOperatorNet<'T>(orch, subnets:SimpleNet<'T>[]) =
    inherit OperatorNet<'T>(orch, subnets)

    member val SubMatching = new HashSet<_>()

    override this.Start(args) =
      if this.Active.Add(-1) then
        if this.Subnets.Length = 0 then
          this.SetMatching(this.Matching, args)
        else
          for i = 0 to this.Subnets.Length-1 do
            this.SubStart(i, args)
        (this :> INotifiable<_>).NotifyDeactivation(-1, args)

    override this.SubMatch(aux, args) =
      this.SubMatching.Add(aux) |> ignore
      if this.Matching then
        this.Orch.EnqueueOpEval(this)

    override this.SubUnmatch(aux, args) =
      this.SubMatching.Remove(aux) |> ignore
      if not this.Matching then
        this.Orch.EnqueueOpEval(this)


  [<StructuredFormatDisplay("{AsString}")>]
  type AnyNet<'T>(orch, subnets) =
    inherit CommutativeOperatorNet<'T>(orch, subnets)

    member private this.AsString = opString this.Subnets " ||| " "nil"

    override this.Matching = this.SubMatching.Count <> 0


  [<StructuredFormatDisplay("{AsString}")>]
  type AllNet<'T>(orch, subnets) =
    inherit CommutativeOperatorNet<'T>(orch, subnets)

    member private this.AsString = opString this.Subnets " &&& " "epsilon"

    override this.Matching = this.SubMatching.Count = this.Subnets.Length

  type Expr<'T> =
    | SimpleEvent of IEvent<'T> * ('T -> bool)
    | Restrict of Expr<'T> * IEvent<'T>[]
    | Iter of Expr<'T>
    | Any of Expr<'T>[]
    | All of Expr<'T>[]
    | Cat of Expr<'T>[]
    | Reaction of Expr<'T> * ('T -> unit)
    | Finally of Expr<'T> * ('T -> unit)
  with
    static member ( / ) (expr, events) = Restrict(expr, events)
    static member ( ~+ ) (expr) = Iter(expr)
    static member ( - ) (x, y) = Cat([| x ; y |])
    static member ( &&& ) (x, y) = All([| x ; y |])
    static member ( ||| ) (x, y) = Any([| x ; y |])
    static member ( |-> ) (expr, f) = Reaction(expr, f)
    static member ( |=> ) (expr, f) = Finally(expr, f)

  module Expr =
    let epsilon = All [| |]
    let never = Any [| |]

    let trueP _ = true

    let ( !! ) x = SimpleEvent(x, trueP)
    let ( %- ) x y = SimpleEvent(x, y)

    let rec compile (orch:Orchestrator<'T>) restriction expr : SimpleNet<'T> =
      match expr with
        | SimpleEvent(e, p) -> new GroundTermNet<_>(orch, p, e, restriction) :>  _
        | Restrict(subexpr, events) ->
          let events = new HashSet<_>(events)
          events.UnionWith(restriction)
          compile orch events subexpr

        | All(subexprs) -> new AllNet<_>(orch, Array.map (compile orch restriction) subexprs) :>  _
        | Any(subexprs)  -> new  AnyNet<_>(orch, Array.map (compile orch restriction) subexprs) :>  _
        | Cat(subexprs) -> new CatNet<_>(orch, Array.map (compile orch restriction) subexprs) :>  _

        | Iter(subexpr) -> new IterNet<_>(orch, compile orch restriction subexpr) :>  _

        | Reaction(subexpr, f) -> new ReactNet<_>(orch, compile orch restriction subexpr, f) :>  _
        | Finally(subexpr, f) -> new FinallyNet<_>(orch, compile orch restriction subexpr, f) :>  _


    let simple event =
      SimpleEvent(event, trueP)

    let cond event pred =
      SimpleEvent(event, pred)

    let restrict bounds subexpr =
      Restrict(subexpr, bounds)

    let all ([<ParamArray>] subexprs) =
      All subexprs

    let any ([<ParamArray>] subexprs) =
      Any subexprs

    let cat ([<ParamArray>] subexprs) =
      Cat subexprs

    let iter expr =
      Iter expr

    let react f expr =
      Reaction(expr, f)

    let finallyDo f expr =
      Finally(expr, f)


    let condInvoke event (pred:Predicate<_>) =
      SimpleEvent(event, pred.Invoke)

    let reactInvoke (action:Action<_>) expr =
      Reaction(expr, action.Invoke)

    let finallyDoInvoke (action:Action<_>) expr =
      Finally(expr, action.Invoke)


    type RootNotifiable<'T>(net:IDisposable) =
      interface INotifiable<'T> with
        member this.NotifyDeactivation(aux, args) =
          net.Dispose()

        member this.NotifyMatch(aux, args) = ()
        member this.NotifyUnmatch(aux, args) = ()

    let start args orch expr =
      let empty = new HashSet<_>()
      let net = compile orch empty expr
      net.Parent <- new RootNotifiable<_>(net)
      net.Initialize()
      net.Start(args)
      orch.EnqueueEvent orch Unchecked.defaultof<_> null args
      net :> IDisposable

    let stop (net:IDisposable) =
      net.Dispose()

  module Orchestrator =
    let create () =
      new Orchestrator<_>()

    let createDebug () =
      new DebugOrchestrator<_>()
