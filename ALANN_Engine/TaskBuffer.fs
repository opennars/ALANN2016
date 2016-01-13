module TaskBuffer

open System
open System.Collections.Generic
open Akka
open Akka.FSharp
open Akka.Actor
open ALANN
open Types
open Messages

type State = {Buffer : Queue<Task>; TaskDispatcher : ActorSelection}

let taskBuffer (state : State) (mailbox : Actor<GeneralMessage>) = function
    | NewTask(task) 
        -> state.Buffer.Enqueue task

    | DispatchBuffer
        -> let count = state.Buffer.Count
           if count > Parameters.Parameters.NEW_TASKS_PER_CYCLE then
               [for t in state.Buffer -> t]
               |> List.sortByDescending (fun t -> t.AV)
               |> List.take Parameters.Parameters.NEW_TASKS_PER_CYCLE
               |> List.iter (NewTask >> state.TaskDispatcher.Tell)
            else
               [for t in state.Buffer -> t]
               |> List.iter (NewTask >> state.TaskDispatcher.Tell)

           state.Buffer.Clear()

           let discarded = max (count - Parameters.Parameters.NEW_TASKS_PER_CYCLE) 0
           if discarded > 0 then
               printfn "TaskBuffer -> Discarding(%d) Tasks" discarded

           mailbox.Sender() <! true

    | Reset
        -> state.Buffer.Clear()

    | _ -> failwith "Unexpected message"
        
let taskBufferRef = spawn system "taskBuffer" (actorOf2 (taskBuffer ({Buffer = new Queue<Task>(); TaskDispatcher = select "Akka://ALANN/user/taskDispatcher" system})))

