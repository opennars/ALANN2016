module Inference

open System
open Akka.Actor
open Akka.FSharp
open ALANN
open EventFunctions
open Messages
open Trail

type InferenceState = {TaskBuffer : ActorSelection; mutable Count : int}

let inferenceWorker (state : InferenceState) (mailbox: Actor<InferenceMsg>) = function
    | InferenceReq(task, belief) 
        -> state.Count <- state.Count + 1
           if nonOverlappingEvidence task.Stamp.Trail belief.Stamp.Trail &&
              nonOverlappingDerivations task.Stamp.DerivationTrail belief.Stamp.DerivationTrail 
              then publish (InferenceRuleReq(task, belief, state.TaskBuffer)) mailbox.Context.System.EventStream |> ignore

    | ResetCounter 
        -> raiseInferenceLengthEvent(state.Count)
           state.Count <- 0

let inferenceRef = spawn system "inference" (actorOf2 (inferenceWorker ({TaskBuffer = select "Akka://ALANN/user/taskBuffer" system; Count = 0})))

system.Scheduler.ScheduleTellRepeatedly(TimeSpan.FromSeconds(1.0), TimeSpan.FromSeconds(0.25), inferenceRef, InferenceMsg.ResetCounter)

