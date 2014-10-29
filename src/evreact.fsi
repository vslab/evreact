namespace EvReact
  open System

  type Event<'T> =
    new : unit -> Event<'T>
    new : string -> Event<'T>
    member Trigger : 'T -> unit
    member Publish : IEvent<'T>

  module Event =
    val create : string -> Event<'a>


  type Orchestrator<'T> =
    class
    end

  type DebugOrchestrator<'T> =
    class
      inherit Orchestrator<'T>
      member IsEmpty : bool
      [<CLIEvent>] member OnEvent : IEvent<IEvent<'T> * obj * 'T>
      [<CLIEvent>] member OnStepBegin : IEvent<IEvent<'T> * obj * 'T>
      [<CLIEvent>] member OnStepEnd : IEvent<IEvent<'T> * obj * 'T>
    end

  module Orchestrator =
    val create : unit -> Orchestrator<'a>
    val createDebug : unit -> DebugOrchestrator<'a>


  [<Sealed>]
  type Expr<'T> =
    static member ( |=> ) : Expr<'a> * ('a -> unit) -> Expr<'a>
    static member ( |-> ) : Expr<'a> * ('a -> unit) -> Expr<'a>
    static member ( &&& ) : Expr<'a> * Expr<'a> -> Expr<'a>
    static member ( ||| ) : Expr<'a> * Expr<'a> -> Expr<'a>
    static member (  -  ) : Expr<'a> * Expr<'a> -> Expr<'a>
    static member (  /  ) : Expr<'a> * IEvent<'a>[] -> Expr<'a>
    static member (  ~+ ) : Expr<'a> -> Expr<'a>

  module Expr =
    val ( !! ) : IEvent<'a> -> Expr<'a>
    val ( %- ) : IEvent<'a> -> ('a -> bool) -> Expr<'a>

    val epsilon : Expr<'a>
    val never : Expr<'a>

    val simple : IEvent<'a> -> Expr<'a>
    val cond : IEvent<'a> -> ('a -> bool) -> Expr<'a>

    val all : [<ParamArray>] subexprs:Expr<'a> [] -> Expr<'a>
    val any : [<ParamArray>] subexprs:Expr<'a> [] -> Expr<'a>
    val cat : [<ParamArray>] subexprs:Expr<'a> [] -> Expr<'a>
    val iter : Expr<'a> -> Expr<'a>
    val restrict : IEvent<'a> [] -> Expr<'a> -> Expr<'a>
    val react : ('a -> unit) -> Expr<'a> -> Expr<'a>
    val finallyDo : ('a -> unit) -> Expr<'a> -> Expr<'a>

    val start : 'a -> Orchestrator<'a> -> Expr<'a> -> IDisposable
    val stop : IDisposable -> unit

    val condInvoke : IEvent<'a> -> Predicate<'a> -> Expr<'a>
    val reactInvoke : Action<'a> -> Expr<'a> -> Expr<'a>
    val finallyDoInvoke : Action<'a> -> Expr<'a> -> Expr<'a>
