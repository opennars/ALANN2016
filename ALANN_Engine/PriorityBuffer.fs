module PriorityBuffer


open System.Collections.Generic
open System.Collections
open C5
open Types

type IPriorityBuffer =
        
        abstract Insert : Task -> unit
        abstract RemoveMax : unit -> Task
        abstract Clear : unit -> unit
        abstract GetEnumerator : unit -> IEnumerator<Task>
        abstract Count : int 

type TaskPriorityComparer() =
    interface IComparer<Task> with
        member x.Compare(x1 : Task, y: Task): int = x1.AV.CompareTo(y.AV)

type PriorityBuffer(n : int) =
    let q = IntervalHeap<Task>(n, TaskPriorityComparer()) :> IPriorityQueue<Task>

    let maxSize = n

    interface IPriorityBuffer with
        
        member x.Insert(task) =

            if q.Count >= maxSize then
                let (deleted, h) = q.DeleteMin()
                let h = ref h
                q.Add(h, task) |> ignore

            else
                let h = ref null
                q.Add(h, task) |> ignore

        member x.RemoveMax() =

            let (removed, h) = q.DeleteMax()
            removed


        member x.Clear() =
        
            while not(q.IsEmpty) do
                let (_,_) = q.DeleteMax()
                ()

        member x.GetEnumerator() : IEnumerator<Task> = q.GetEnumerator()

        member x.Count : int = q.Count
