namespace EvReact

  open Expr
  open System
  open System.Threading
  open System.Collections.Generic

  module Utils =
    let start0 orch expr = start Unchecked.defaultof<_> orch expr

    type Demultiplexer<'K, 'T when 'K : equality>(evt:IEvent<'T>, f) =
      let noNet = Unchecked.defaultof<_>
      let noEvt = Unchecked.defaultof<_>

      let mutable net = noNet
      let dict = Dictionary<'K,WeakReference>()

      let get key =
        let mutable wr = Unchecked.defaultof<_>
        if dict.TryGetValue(key, &wr) then
          wr.Target :?> DemultiplexEvent<_,_>
        else
          noEvt

      let demux _ args =
        let locked () = get (f args)
        let e = lock dict locked
        if e <> noEvt then
          e.Trigger(args)

      let demux = Handler<_>(demux)

      let unsubscribe self key () =
        dict.Remove(key) |> ignore
        if dict.Count = 0 then
          evt.RemoveHandler(demux)

      let subscribe self key () =
        if dict.Count = 0 then
          evt.AddHandler(demux)
        let mutable e = get key
        if e = noEvt then
          e <- new DemultiplexEvent<_,_>(self, key)
          dict.[key] <- WeakReference(e)
        e.Publish

      member x.Unsubscribe(key) = lock dict <| unsubscribe x key
      member x.Item with get(key) = lock dict <| subscribe x key

    and DemultiplexEvent<'K, 'T when 'K : equality>(demux:Demultiplexer<'K,'T>, key: 'K) =
      inherit Event<'T>(key.ToString())

      override x.Finalize() = demux.Unsubscribe(key)
