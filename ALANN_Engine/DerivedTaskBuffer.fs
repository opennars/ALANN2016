module DerivedTaskBuffer

open System
open System.Collections.Generic
open Akka
open Akka.FSharp
open Akka.Actor
open ALANN
open Types
open Messages
open PriorityQueue

type State = {Buffer : PriorityQueue; TaskDispatcher : ActorSelection}

let derivedTaskBuffer (state : State) (mailbox : Actor<GeneralMessage>) = function
    | NewTask(task) 
        -> state.Buffer.Insert(task) |> ignore
//           if state.Buffer.Count > Parameters.Parameters.NEW_TASKS_PER_CYCLE then
//              state.Buffer.RemoveMin() |> ignore

    | BufferNextTask
        -> let count = min state.Buffer.Count 100 //Parameters.Parameters.INFERENCE_ACTORS
           if state.Buffer.Count > 0 then printfn "derived tasks = %d" state.Buffer.Count
           for i in 1..count do
               state.Buffer.RemoveMax() |> (NewTask >> state.TaskDispatcher.Tell)
           
    | Reset
        -> state.Buffer.Clear()

    | _ -> failwith "Unexpected message"
        
let derivedTaskBufferRef = spawn system "derivedTaskaskBuffer" (actorOf2 (derivedTaskBuffer ({Buffer = new PriorityQueue(Parameters.Parameters.NEW_TASKS_PER_CYCLE); TaskDispatcher = select "Akka://ALANN/user/taskDispatcher" system})))

