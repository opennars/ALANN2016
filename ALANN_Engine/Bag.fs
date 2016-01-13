module Bag

open System
open System.Collections.Generic
open Wintellect.PowerCollections
open IStore
open NameTable
open PriorityTable
open System.Diagnostics

/// <summary>
/// 
/// </summary>
type Bag<'K, 'T when 'K : equality and 'T :> IStorable<'K>>(capacity : int, selectionFactor : float) =
    static let rnd = Random()
    let avComparer = { new IComparer<'T> with
                                 member this.Compare(l, r) =
                                    match l.P - r.P with
                                    | i when i > 0.0f -> -1
                                    | i when i < 0.0f ->  1
                                    | _               ->  0 }

    let keyIndex = NameTable<'K, 'T>(capacity)
    let avIndex = PriorityTable<'K, 'T>(capacity, 50, selectionFactor) :> IPriorityTable<'K, 'T>
    
    interface IStore<'K, 'T> with
        /// <summary>
        /// 
        /// </summary>
        member x.GetEnumerator(): Collections.IEnumerator = keyIndex.Values.GetEnumerator() :> Collections.IEnumerator
        
        member x.Count: int = keyIndex.Count
        member x.GetEnumerator(): IEnumerator<'T> = keyIndex.Values.GetEnumerator() :> IEnumerator<'T>
        /// <summary>
        /// 
        /// </summary>
        /// <param name="key"></param>
        member this.Contains(key : 'K) =
            keyIndex.ContainsKey key

        member this.Insert(item : 'T) =
            match keyIndex.ContainsKey item.Key with
            | true -> failwith "Bag.Insert(): key exists"
            | false -> 
                keyIndex.Add(item.Key, item)
                let oldItem = avIndex.Insert(item)

                // if capacity is exceeded then 
                // remove lowest priority item from bag
                let count = keyIndex.Count
                if count > capacity then
                    match oldItem with
                    | Some oldItem -> keyIndex.Remove(oldItem.Key) |> ignore
                    | None -> failwith "Bag.Insert : expected oldItem"
                oldItem
                    
        member this.TakeOut() =
            let count = avIndex.Count
            if count = 0 then
                None
            else
                let item = avIndex.Select()
                match keyIndex.Remove(item.Key) with
                | false -> failwith "Bag.TakeOut(): key does not exist"
                | true -> ()
                Some item

        member this.PutBack(item : 'T) =
            keyIndex.Add(item.Key, item)
            avIndex.PutBack(item)

        member this.TakeOutByKey(key) =
            let item = keyIndex.[key]
            match keyIndex.Remove(key) with
            | false -> failwith "Bag.TakeOutBykey(): key does not exist"
            | true -> ()

            match avIndex.Remove(item) with
            | true -> item
            | false -> failwith "Remove(item): failed"

        member this.FindByKey(key) = keyIndex.[key]
//        member this.TryGetValue(key) = keyIndex.TryGetValue(key)

        member this.Clear() =
            keyIndex.Clear()
            avIndex.Clear()

//        member this.GetItemsInPriorityOrder(n) =
//            avIndex.GetItemsInPriorityOrder(n)


type Bag2<'K, 'T when 'K : equality and 'T :> IStorable<'K>>(capacity : int, selectionFactor : float) =
    static let rnd = Random()
    let avComparer = { new IComparer<'T> with
                                 member this.Compare(l, r) =
                                    match l.P - r.P with
                                    | i when i > 0.0f -> -1
                                    | i when i < 0.0f ->  1
                                    | _               ->  0 }

    let keyIndex = NameTable<'K, 'T>(capacity)
    let avIndex = BigList<'T>()

    interface IStore<'K, 'T> with
        member x.GetEnumerator(): Collections.IEnumerator = keyIndex.Values.GetEnumerator() :> Collections.IEnumerator
        
        member x.Count: int = avIndex.Count
        member x.GetEnumerator(): IEnumerator<'T> = keyIndex.Values.GetEnumerator() :> IEnumerator<'T>

        member this.Contains(key : 'K) =
            keyIndex.ContainsKey key

        member this.Insert(item : 'T) =
            match keyIndex.ContainsKey item.Key with
            | true -> failwith "Bag.Insert(): key exists"
            | false -> 
                keyIndex.Add(item.Key, item)
                let i = avIndex.BinarySearch(item, avComparer)
                let i = if i < 0 then ~~~i else i
                avIndex.Insert(i, item)

                // if capacity is exceeded then 
                // remove lowest priority item from bag
                let count = avIndex.Count
                if count > capacity then
                    let removed = avIndex.[count - 1]
                    avIndex.RemoveAt(count - 1)
                    match keyIndex.Remove(removed.Key) with
                    | false -> failwith "Bag.TakeOut(): key does not exist"
                    | true -> ()
                    Some removed
                else
                    None
                    
        member this.TakeOut() =
            let count = float(avIndex.Count)
            if count = 0.0 then
                None
            else
                let i = int32(Math.Pow(rnd.NextDouble(), selectionFactor) * count)

                let item = avIndex.[i]
                match keyIndex.Remove(item.Key) with
                | false -> failwith "Bag.TakeOut(): key does not exist"
                | true -> ()
                avIndex.RemoveAt(i)
                Some item

        member this.PutBack(item : 'T) =
            keyIndex.Add(item.Key, item)
            let i = avIndex.BinarySearch(item, avComparer) 
            let i = if i < 0 then ~~~i else i
            avIndex.Insert(i, item)

        member this.TakeOutByKey(key) =
            let item = keyIndex.[key]
            match keyIndex.Remove(key) with
            | false -> failwith "Bag.TakeOutBykey(): key does not exist"
            | true -> ()

//            // search for first matching P value
//            // i - 1 as binary search may find second match first
//            let mutable i = max (avIndex.BinarySearch(item, avComparer) - 1) 0
//            // loop through index until key is found
//            // as can be multiple P's with same value
//            while avIndex.[i].Key <> key do
//                i <- i + 1
//            avIndex.RemoveAt(i)

            // TODO *** Temp for testing
            match avIndex.Remove(item) with
            | true -> item
            | false -> failwith "Bag2.TakeOutByKey() : failed to remove from avIndex"

        member this.FindByKey(key) = keyIndex.[key]
//        member this.TryGetKey(key) = keyIndex.TryGetValue(key)

        member this.Clear() =
            keyIndex.Clear()
//            avIndex.Clear()

