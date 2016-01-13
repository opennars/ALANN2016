module PriorityTable

open System
open System.Collections.Generic
open Wintellect.PowerCollections
open IStore

type IPriorityTable<'K, 'T when 'K : equality and 'T :> IStorable<'K>> =

    abstract Count : int
    abstract Insert : 'T -> 'T option
    abstract PutBack : 'T -> unit
    abstract Remove : 'T -> bool
    abstract Select : unit -> 'T
    abstract Clear : unit -> unit
    abstract GetItemsInPriorityOrder : int32 -> 'T seq

type PriorityTable<'K, 'T when 'K : equality and 'T :> IStorable<'K>>(capacity : int32, levels : int32, selectionFactor : float) =
    static let rnd = Random()

    let mutable count = 0
    let levels = levels
    let table = [| for level in 0..levels - 1 -> new Deque<'T>()|]

    let getLevel (item : 'T) =
        let level =
            ceil(item.P * single(levels)) - 1.0f
            |> int32

        if level < 0 then 0 else level

    let rec selectLevel levels = 
        int32(Math.Pow(rnd.NextDouble(), selectionFactor) * float(levels))

    interface IPriorityTable<'K, 'T> with
        member x.Count = count

        member x.Insert(item) =
            let inlevel = getLevel item
            if count + 1 > capacity then
                let mutable outlevel = 0
                while table.[outlevel].Count = 0 do
                    outlevel <- outlevel + 1

                if outlevel > inlevel then
                    Some item
                else
                    table.[inlevel].AddToBack(item)
                    Some (table.[outlevel].RemoveFromFront())
            else
                table.[inlevel].AddToBack(item)
                count <- count + 1
                None

        member x.PutBack(item) =
            let inlevel = getLevel item
            table.[inlevel].AddToBack(item)
            count <- count + 1

        member x.Remove(item) = 
           if table.[getLevel item].Remove(item)
           then count <- count - 1; true
           else false

        member x.Select() = 
            let mutable level = selectLevel levels
            while table.[level].Count = 0 do
                level <- (level + 1) % levels

            count <- count - 1
            table.[level].RemoveFromFront()

        member x.Clear() = 
            Array.Clear(table, 0, levels)
            count <- 0

        member x.GetItemsInPriorityOrder(n : int32) =
            let mutable items = Seq.empty
            for level in 0..levels - 1 do
                items <- Seq.append items (table.[level].ToArray())
        
            items

