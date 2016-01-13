module TaskDispatcher

open System
open ALANN
open Types
open EventFunctions
open Messages
open TaskDispatcherFunctions
open Akka.FSharp
open Metrics
open InferenceActors

let taskDispatcher state (mailbox : Actor<GeneralMessage>) = function
    | NewTask(task) 
        -> state.Count <- state.Count + 1
           dispatchTaskToTerms state task

    | UpdateConceptTree(n) 
        -> let concepts = getConcepts state.Concepts
//           let mutable maxlen = 0
//           for c in concepts do
//                for b in c.Beliefs do
//                    let len = b.Stamp.Trail.Length
//                    if len > maxlen then 
//                        maxlen <- len
//
//           printfn "Max trail len = %d" maxlen
           raiseUpdateConceptTreeEvent(concepts)

    | ResetMetrics
        -> raiseNewTasksEvent(state.Count * 10) // convert to tasks/sec
           raiseConceptCountEvent(state.Concepts.Count) 
           metricsUpdaterRef <! UpdateMetricMsg(Metric(MetricType.DispatchCount, (state.Count * 4).ToString()))
           state.Count <- 0

    | Reset
        -> [for c in state.Concepts -> c]
           |> List.iter  (fun c -> c.Ref <! Stop)
           state.Concepts.Clear()
           state.Id <- 0
           state.Count <- 0

    | _ -> ()

let taskDispatcherRef = spawn system "taskDispatcher" (actorOf2 (taskDispatcher(newTaskDispatcherState())))

system.Scheduler.ScheduleTellRepeatedly(TimeSpan.FromSeconds(0.0), TimeSpan.FromMilliseconds(100.0), taskDispatcherRef, GeneralMessage.ResetMetrics)

RuleBuilderForward() |> ignore
