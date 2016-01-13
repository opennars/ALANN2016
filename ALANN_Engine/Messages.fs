module Messages

open Types
open Akka.Actor

type GeneralMessage    = | NewTask           of Task
                         | Primer            of Task * single
                         | UpdateConceptTree of int 
                         | ParseFile         of string
                         | ParseText         of string 
                         | UpdateQuestions   of Task
                         | ConceptInference
                         | UpdateAV
                         | GetConceptState
                         | ResetDispatcher
                         | DispatchBuffer
                         | BufferNextTask 
                         | ResetMetrics
                         | Reset
                         | Stop
                       
                         | ActivateConcept   of IActorRef
                         | ProcessConcepts
                         | ActivationComplete

type SubscriberMessage = | InferenceReq      of Task * Task
                         | SubscribeToInferenceReq
                         | DisableInferenceLogging
                         | EnableInferenceLogging
