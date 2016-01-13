module QuestionUpdater

open System.Collections.Generic
open Akka
open Akka.FSharp
open Akka.Actor
open ALANN
open Messages
open Types
open Parameters
open Wintellect.PowerCollections
open EventFunctions

type QuestionUpdaterState = {ResultBuffer : Deque<Task>}

let questionUpdaterWorker (state : QuestionUpdaterState) (mailbox: Actor<GeneralMessage>) = function
    | UpdateQuestions(task)
        -> match state.ResultBuffer.Contains task with
           | true ->
               let i = state.ResultBuffer.IndexOf task
               state.ResultBuffer.Insert(i, task)
               raiseUserQuestionAnswerEvent(task)

           | false
               -> state.ResultBuffer.AddToBack task
                  if state.ResultBuffer.Count > Parameters.COMMAND_BUFFER_MAX then
                      state.ResultBuffer.RemoveFromFront() |> ignore
                  raiseUserQuestionAnswerEvent(task)

    | _ -> failwith "questionUpdaterWorker : Unexpected Message"

let questionUpdaterRef = spawn system "questionUpdater" (actorOf2 (questionUpdaterWorker({ResultBuffer = new Deque<Task>()}) ))