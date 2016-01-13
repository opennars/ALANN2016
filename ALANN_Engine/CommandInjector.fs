module CommandInjector

open System.Collections.Generic
open Akka
open Akka.FSharp
open Akka.Actor
open ALANN
open Messages
open TaskBuffer
open Types
open Parameters
open Wintellect.PowerCollections
open PriorityBuffer

type CommandInjectorState = {CommandBuffer : Deque<Task>}

let commandInjectorWorker (state : CommandInjectorState) (mailbox: Actor<GeneralMessage>) = function
    | NewTask(task)
        -> state.CommandBuffer.AddToBack task // to maintain input order
           if state.CommandBuffer.Count > Parameters.COMMAND_BUFFER_MAX then
               state.CommandBuffer.RemoveFromFront() |> ignore

    | BufferNextTask
        -> if state.CommandBuffer.Count > 0 then
               let task = state.CommandBuffer.RemoveFromFront()
               taskBufferRef <! NewTask(task)
               state.CommandBuffer.AddToBack task //{ task with AV = task.AV * 0.99f }

    | Reset
        -> state.CommandBuffer.Clear()
        
    | _ -> failwith "commandInjectorWorker : Unexpected Message"

let commandInjectorRef = spawn system "commandInjector" (actorOf2 (commandInjectorWorker({CommandBuffer = new Deque<Task>()}) ))
let injectCommand task = commandInjectorRef <! NewTask(task)
