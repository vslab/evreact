namespace EvReact
  open System

  module Utils =
    val start0 : Orchestrator<'a> -> expr:Expr<'a> -> IDisposable

    type Demultiplexer<'K,'T when 'K : equality> =
      class
        new : IEvent<'T> * ('T -> 'K) -> Demultiplexer<'K,'T>
        member Item : 'K -> IEvent<'T> with get
      end
