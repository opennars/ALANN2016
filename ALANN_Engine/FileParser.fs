module FileParser

open System.Collections.Generic
open Akka.FSharp
open ALANN
open ParserUtils
open Messages
open TaskBuffer
open Types
open CommandInjector
open Utilities

let fileParserWorker (state : FileParserState) (mailbox: Actor<GeneralMessage>) = function
    | ParseFile(filename) 
        -> let tasks = parseFileToTasks(filename)
           tasks |> List.iter state.Buffer.Enqueue
//           tasks |> List.iter injectCommand
           tasks |> List.iter (fun t -> if t |> isCommand then t |> injectCommand)

    | BufferNextTask
        -> if state.Buffer.Count > 0 then
               taskBufferRef <! NewTask(state.Buffer.Dequeue())

    | Reset
        -> state.Buffer.Clear()
        
    | _ -> failwith "fileParserWorker : Unexpected Message"

let fileParserRef = spawn system "fileParser" (actorOf2 (fileParserWorker({Buffer = new Queue<Task>()}) ))
