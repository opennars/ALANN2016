module Events

open System
open Types

type ParseResultEventArgs(sentence) =
       inherit EventArgs()
       member x.ParsedSentence = sentence

type ParseErrorEventArgs(error : string) =
    inherit EventArgs()
    member x.Error = error

type InferenceInfoEventArgs(inferenceResult, inferenceTask : string, inferenceBelief : string, instance : string, inferenceTime: string) =
    inherit EventArgs()
    member x.InferenceResult = inferenceResult
    member x.InferenceTask = inferenceTask
    member x.InferenceBelief = inferenceBelief
    member x.Instance = instance
    member x.InferenceCycle = inferenceTime

type StatusUpdateEventArgs(text : string) =
    inherit EventArgs()
    member x.Text = text

type TaskCompletedEventArgs(text : string) =
    inherit EventArgs()
    member x.Text = text

type InferenceAnswerEventArgs(sentence : string) =
    inherit EventArgs()
    member x.Answer = sentence

type NewTasksEventArgs(numNewTasks : int) =
    inherit EventArgs()
    member x.NumNewTasks = numNewTasks


type ThresholdEventArgs(threshold : float32) =
    inherit EventArgs()
    member x.Threshold = threshold

type InferenceLengthEventArgs(length : int) =
    inherit EventArgs()
    member x.Length = length

type UserQuestionAnswerEventArgs(results : Task) =
    inherit EventArgs()
    member x.Answer = results

type UpdateConceptTreeEventArgs(terms : ConceptState seq) =
    inherit EventArgs()
    member x.Terms = terms

type UpdateMetricsEventArgs(metrics : Metric array) =
    inherit EventArgs()
    member x.Metrics = metrics

