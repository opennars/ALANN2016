module ITaskStore

open System.Collections.Generic
open Types

type ITaskStore =

    abstract Contains : Task -> bool
    abstract Insert : Task -> unit
    abstract Update : Task -> unit
    abstract Merge : Task -> unit
    abstract Apply : (Task * Task -> Task) * Task -> Task
    abstract TryGetValue : Task -> Task option
    abstract Clear : unit -> unit
    abstract Count : int
    abstract GetEnumerator : unit -> IEnumerator<Task>