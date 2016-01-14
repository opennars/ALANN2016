module Concept

open System
open Akka.FSharp
open Akka.Actor
open Types
open Parameters
open ALANN
open Messages
open ConceptFunctions
open Providers
open Factory2
open TaskStore
open ActiveConceptProcessor

let conceptWorker (state : ConceptState) (mailbox: Actor<GeneralMessage>) msg = 
    let eventStream = mailbox.Context.System.EventStream
    if not(state.Subscribed) then
        subscribe typeof<GeneralMessage> mailbox.Self eventStream |> ignore

    match msg with
    | NewTask(task) 
        -> let task = localInference task state
           insertTask state task
           updateActivation task state
           if canActivate state  then
               state.Active <- true
               activeConceptProcessorRef <! ActivateConcept(mailbox.Self)

    | ConceptInference
        -> activate state mailbox
           mailbox.Sender() <! true

    | UpdateAV
        -> decayBeliefs state
           decayActivation state
               
    | GetConceptState 
        -> //state.Tasks.Insert state.Task
           mailbox.Sender() <! state

    | Stop 
        -> mailbox.Context.Stop(mailbox.Self)

    | Primer(belief, spike) 
        -> handlePrimer belief spike state 

    | _ -> failwith "Unexpected message"

let makeConcept id term =

    let ref =
        spawn system (id.ToString()) 
            (actorOf2 (conceptWorker ({ Name = term
                                        Tasks = TaskStore(Parameters.TASK_CAPACITY)
                                        Beliefs = TaskStore(Parameters.BELIEF_CAPACITY)
                                        Activation = 0.0f
                                        Priming = 1.0f
                                        LastActivation = SystemTime.Now
                                        LastUpdate = SystemTime.Now
                                        Active = false
                                        Subscribed = false
                                        TaskBuffer = select "Akka://ALANN/user/taskBuffer" system})))

    { Term = term; Ref = ref; Activations = 0}
