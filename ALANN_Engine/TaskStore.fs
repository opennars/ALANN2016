module TaskStore

open System.Collections.Generic
open C5
open Types
open Providers

type TaskPriorityComparer() =
    interface IComparer<Task> with
        member x.Compare(x1 : Task, y: Task): int = x1.AV.CompareTo(y.AV)

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

type TaskStore(n : int) =
    let q = IntervalHeap<Task>(n, ActivationFrequencyComparer()) :> IPriorityQueue<Task>
    let d = HashDictionary<Task, IPriorityQueueHandle<Task>>() :>IDictionary<Task, IPriorityQueueHandle<Task>>

    let merge a b = a // TODO *** complete

    let maxSize = n

    interface ITaskStore with
        
        member x.Contains(term) = d.Contains term

        member x.Insert(task) =

            if d.Count >= maxSize then
                let (deleted, h) = q.DeleteMin()
                let h = ref h
                match d.Remove(deleted, h ) with
                | true -> ()
                | false -> failwith "TaskStore.Insert() : failed to remove on maxSize"

                q.Add(h, task) |> ignore
                d.Add(task, !h)

            else
                let h = ref null
                q.Add(h, task) |> ignore
                d.Add(task, !h)

        member x.Update(task) =

            if not(d.Contains(task) ) then
                failwith "TaskStore.Update() : task does not exist"

            let h = d.[task]
            q.[h].AV <- task.AV

        member x.Merge(newTask) =
            
            if not(d.Contains(newTask) ) then
                failwith "TaskStore.Merge() : task does not exist"

            let h = d.[newTask]
            let oldTask = q.[h]
            q.[h] <- merge newTask oldTask

        member x.Apply(f, newTask) =
            
            if not(d.Contains(newTask) ) then
                failwith "TaskStore.Apply() : task does not exist"
            
            let h = d.[newTask]
            let oldTask = q.[h]
            let applied = f (newTask, oldTask)
            q.[h] <- applied
            applied

        member x.RemoveMax() =

            let (removed, h) = q.DeleteMax()
            d.Remove(removed, ref h) |> ignore
            removed

        member x.TryGetValue term = 
            
            if d.Contains(term) then Some(q.[d.[term]])
            else None

        member x.Clear() =
        
            d.Clear()
            while not(q.IsEmpty) do
                let (_,_) = q.DeleteMax()
                ()

        member x.GetEnumerator() : IEnumerator<Task> = q.GetEnumerator()

        member x.Count : int = d.Count
