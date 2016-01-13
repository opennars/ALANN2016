module SentenceParser

open System.Collections.Generic
open Akka.FSharp
open ALANN
open ParserUtils
open Messages
open Types
open Utilities
open CommandInjector
open TaskBuffer

let sentenceParserWorker (state : SentenceParserState) (mailbox: Actor<GeneralMessage>) = function
    | ParseText(str) 
        -> let tasks = parseTextToTasks(str)
           tasks |> Array.iter state.Buffer.Enqueue
//           tasks |> Array.iter injectCommand
           tasks |> Array.iter (fun t -> if t |> isCommand then t |> injectCommand)

    | BufferNextTask
        -> if state.Buffer.Count > 0 then
               taskBufferRef <! NewTask(state.Buffer.Dequeue())

    | Reset
        -> state.Buffer.Clear()
       
    | _ -> failwith "sentenceParserWorker : Unexpected Message"
        
let sentenceParserRef = spawn system "sentenceParser" (actorOf2 (sentenceParserWorker({Buffer = new Queue<Task>()}) ))

