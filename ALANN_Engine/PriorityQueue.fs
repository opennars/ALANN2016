module PriorityQueue

open System.Collections.Generic
open C5
open Types
open Providers

type TaskPriorityComparer() =
    interface IComparer<Task> with
        member x.Compare(x1 : Task, y: Task): int = x1.AV.CompareTo(y.AV)

type PriorityQueue(n : int) =
    let q = IntervalHeap<Task>(n, TaskPriorityComparer()) :> IPriorityQueue<Task>

    member x.Insert(task) = q.Add(task)
    member x.RemoveMax() = q.DeleteMax()
    member x.RemoveMin() = q.DeleteMin()
    member x.Clear() = 
        while not(q.IsEmpty) do
            q.DeleteMax() |> ignore

    member x.Count : int = q.Count

