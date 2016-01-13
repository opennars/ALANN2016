module ConceptStore

open System.Collections.Generic
open C5
open Types

type ConceptRefPriorityComparer() =
    interface IComparer<ConceptRef> with
        member x.Compare(x1 : ConceptRef, y: ConceptRef): int = x1.Activations.CompareTo(y.Activations)

type ConceptStore(n) =
    let q = IntervalHeap<ConceptRef>(n, ConceptRefPriorityComparer()) :> IPriorityQueue<ConceptRef>
    let d = HashDictionary<Term, IPriorityQueueHandle<ConceptRef>>() :>IDictionary<Term, IPriorityQueueHandle<ConceptRef>>

    let maxSize = n

    member x.Contains(term) = d.Contains term

    member x.Insert(conceptRef) =

        if d.Count >= maxSize then
            let (deleted, h) = q.DeleteMin()
            let h = ref h
            match d.Remove(deleted.Term, h ) with
            | true -> ()
            | false -> failwith "ConceptStore.Insert() : failed to remove on maxSize"

            q.Add(h, conceptRef) |> ignore
            d.Add(conceptRef.Term, !h)
            Some deleted
        else
            let h = ref null
            q.Add(h, conceptRef) |> ignore
            d.Add(conceptRef.Term, !h)
            None

    member x.Update(conceptRef) =

        if not(d.Contains(conceptRef.Term) ) then
            failwith "ConceptStore.Update() : conceptRef does not exist"

        let h = d.[conceptRef.Term]
        q.[h].Activations <- conceptRef.Activations

    member x.TryGetValue term = 
        
        if d.Contains(term) then Some(q.[d.[term]])
        else None

    member x.Clear() =
    
        d.Clear()
        while not(q.IsEmpty) do
            let (_,_) = q.DeleteMax()
            ()

    member x.GetEnumerator() = q.GetEnumerator()

    member x.Count = d.Count