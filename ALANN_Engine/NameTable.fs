module NameTable

open System
open System.Collections.Generic
open Wintellect.PowerCollections
open IStore

type INameTable<'K, 'T when 'K : equality and 'T :> IStorable<'K>> =
    inherit IDictionary<'K, 'T>
    abstract Insert : 'K * 'T -> 'T option


type NameTable<'K, 'T when 'K : equality and 'T :> IStorable<'K>>(capacity : int) =
    inherit Dictionary<'K, 'T>(capacity, HashIdentity.Structural)

    interface INameTable<'K, 'T> with

        member x.Insert(key, item) =
            match x.ContainsKey key with
            | true
                -> let oldItem = x.[key]
                   x.[key] <- item
                   Some oldItem
            | false
                -> x.Add(key, item)
                   None
