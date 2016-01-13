module EventFunctions

open System
open System.Threading
open AsyncUtils
open Events
open Types
open TypeFormatters
open Providers

let syncContext               = SynchronizationContext.CaptureCurrent()
let InferenceResultEvent      = new Event<InferenceInfoEventArgs>()
let ParseResultEvent          = new Event<ParseResultEventArgs>()
let ParseErrorEvent           = new Event<ParseErrorEventArgs>()
let StatusUpdateEvent         = new Event<StatusUpdateEventArgs>()
let TaskCompletedEvent        = new Event<TaskCompletedEventArgs>()
let InferenceAnswerEvent      = new Event<InferenceAnswerEventArgs>()
let NewTasksEvent             = new Event<NewTasksEventArgs>()
let ConceptCountEvent         = new Event<NewTasksEventArgs>()
let ThresholdEvent            = new Event<ThresholdEventArgs>()
let InferenceLengthEvent      = new Event<InferenceLengthEventArgs>()
let UserQuestionAnswerEvent   = new Event<UserQuestionAnswerEventArgs>()
let UpdateConceptTreeEvent    = new Event<UpdateConceptTreeEventArgs>()
let UpdateMetricsEvent        = new Event<UpdateMetricsEventArgs>()

let raiseInferenceResultEvent (task : Task option) (belief : Task option) (resultList : Task list) =
    let resultArray = resultList |> List.map (fun t -> TypeFormatter.Sentence t.S) |> List.toArray
    syncContext.RaiseEvent InferenceResultEvent (
        InferenceInfoEventArgs(
            resultArray,
            (match task with | Some task -> TypeFormatter.Sentence task.S | _ -> ""),
            (match belief with | Some belief -> TypeFormatter.Sentence belief.S | _ -> ""),
            "---",
            DateTime.UtcNow.ToLongTimeString() + "." + DateTime.UtcNow.Millisecond.ToString())
    )
    
let raiseStatusUpdateEvent(s) = syncContext.RaiseEvent StatusUpdateEvent (StatusUpdateEventArgs(s))

let raiseTaskCompletedEvent(s) = syncContext.RaiseEvent TaskCompletedEvent (TaskCompletedEventArgs(s)) 

let raiseParseResultEvent(s) = syncContext.RaiseEvent ParseResultEvent (ParseResultEventArgs(s))

let raiseParseErrorEvent(s) = syncContext.RaiseEvent ParseErrorEvent (ParseErrorEventArgs(s))

let raiseInferenceAnswerEvent(s) = syncContext.RaiseEvent InferenceAnswerEvent (InferenceAnswerEventArgs(s))

let raiseNewTasksEvent(n) = syncContext.RaiseEvent NewTasksEvent (NewTasksEventArgs(n))

let raiseConceptCountEvent(n) = syncContext.RaiseEvent ConceptCountEvent (NewTasksEventArgs(n))

let raiseThresholdEvent(n) = syncContext.RaiseEvent ThresholdEvent (ThresholdEventArgs(n))

let raiseInferenceLengthEvent(n) = syncContext.RaiseEvent InferenceLengthEvent (InferenceLengthEventArgs(n))

let raiseUserQuestionAnswerEvent(s) = syncContext.RaiseEvent UserQuestionAnswerEvent (UserQuestionAnswerEventArgs(s))

let raiseUpdateConceptTreeEvent(t) = syncContext.RaiseEvent UpdateConceptTreeEvent (UpdateConceptTreeEventArgs(t))

let raiseUpdateMetricsEvent(m) = syncContext.RaiseEvent UpdateMetricsEvent (UpdateMetricsEventArgs(m))

