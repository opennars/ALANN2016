module TaskStore

open System.Collections.Generic
open C5
open Types
open IStore
open Providers

type TaskPriorityComparer() =
    interface IComparer<Task> with
        member x.Compare(x1 : Task, y: Task): int = x1.AV.P.CompareTo(y.AV.P)

type ActivationFrequencyComparer() =
    interface IComparer<Task> with
        member x.Compare(x1 : Task, y: Task): int = 
            // activation frequency = activations / time
            let getTruth task = 
                if task.S.TV |> Option.isSome 
                then task.S.TV.Value 
                else (0.0f, 0.0f)
    
            let now = SystemTime.Now
            let xFreq = single(x1.Stamp.Activations) / single(now - x1.Stamp.Created + 1L) + 1.0f / single(x1.Stamp.SC) + snd(getTruth x1)
            let yFreq = single(y.Stamp.Activations) / single(now - y.Stamp.Created + 1L) + 1.0f / single(y.Stamp.SC) + snd(getTruth y)
            xFreq.CompareTo(yFreq)

type TaskStore(n) =
    let q = IntervalHeap<Task>(n, TaskPriorityComparer()) :> IPriorityQueue<Task>
    let d = HashDictionary<Task, IPriorityQueueHandle<Task>>() :>IDictionary<Task, IPriorityQueueHandle<Task>>

    let maxSize = n

    member x.Contains(term) = d.Contains term

    member x.Insert(task) =

        if d.Count >= maxSize then
            let (deleted, h) = q.DeleteMin()
            let h = ref h
            match d.Remove(deleted, h ) with
            | true -> ()
            | false -> failwith "ConceptStore.Insert() : failed to remove on maxSize"

            q.Add(h, task) |> ignore
            d.Add(task, !h)

        else
            let h = ref null
            q.Add(h, task) |> ignore
            d.Add(task, !h)

    member x.Update(task) =

        if not(d.Contains(task) ) then
            failwith "ConceptStore.Update() : conceptRef does not exist"

        let h = d.[task]
        q.[h].AV.P <- task.AV.P

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

    member x.DeleteMin() = 
        let (deleted, h) = q.DeleteMin()
        let h = ref h
        match d.Remove(deleted, h ) with
        | true -> ()
        | false -> failwith "ConceptStore.DeleteMin() : failed to remove from dictionary"
        deleted