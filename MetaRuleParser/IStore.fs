module IStore

open System.Collections.Generic

type IStorable<'K> =
    abstract member Key : 'K 
    abstract member P : single with get

type IStore<'K, 'T when 'K : equality and 'T :> IStorable<'K>> =

    abstract Contains : 'K -> bool
    abstract Insert : 'T -> 'T option
    abstract TakeOut : unit -> 'T option
    abstract TakeOutByKey : 'K -> 'T
    abstract PutBack : 'T -> unit
    abstract FindByKey : 'K -> 'T
    abstract TryGetValue: 'K -> bool * 'T
    abstract Clear : unit -> unit
    abstract Count : int
    abstract GetItemsInPriorityOrder : int32 -> 'T seq
    inherit IEnumerable<'T> 

type IBuffer<'T> =
    abstract Enqueue : 'T -> unit
    abstract Dequeue : unit -> 'T
    abstract Count : int
