module Buffer

open Wintellect.PowerCollections
open IStore

type Buffer<'T>() =
    let buffer = Deque<'T>()
    interface IBuffer<'T> with
        member x.Enqueue t = buffer.AddToBack t
        member x.Dequeue() = buffer.RemoveFromFront()
        member x.Count with get() = buffer.Count