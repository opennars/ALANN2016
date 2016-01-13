module ActiveConceptProcessor

open System
open Wintellect.PowerCollections
open Akka.FSharp
open Akka.Actor
open ALANN
open Messages
open Parameters
open EventFunctions
open Metrics
open Types

type State = {Buffer : Deque<IActorRef>}

let activeConceptProcessor (state : State) (mailbox : Actor<GeneralMessage>) = function
    | ActivateConcept(conceptRef) 
        -> match state.Buffer.Contains conceptRef with
           | true -> ()
           | false -> state.Buffer.AddToBack conceptRef

    | ProcessConcepts
        -> let activeConcepts = state.Buffer.Count
//           [for r in state.Buffer -> r]
////           |> List.distinct
//           |> List.take (min state.Buffer.Count Parameters.INFERENCE_ACTORS) 
//           |> List.iter (fun r -> r <! ConceptInference)
           [|for r in state.Buffer -> r|]
           |> Array.take (min state.Buffer.Count Parameters.INFERENCE_ACTORS) 
           |> Array.Parallel.iter (fun r -> Async.RunSynchronously(r <? ConceptInference) |> ignore)
           metricsUpdaterRef <! UpdateMetricMsg(Metric(MetricType.ActiveConceptCount,(activeConcepts * int(1000.0 / Parameters.CONCEPTS_ACTIVATIONS_FREQUENCY)).ToString()))
           state.Buffer.Clear()
           mailbox.Sender() <! ActivationComplete

    | Reset
        -> state.Buffer.Clear()

    | _ -> failwith "activeConceptProcessor: Unexpected message"
        
let activeConceptProcessorRef = 
    spawn system "activeConceptProcessor" 
        (actorOf2 (activeConceptProcessor 
            ({ Buffer = new Deque<IActorRef>() })))

//system.Scheduler.ScheduleTellRepeatedly( TimeSpan.FromSeconds(0.0), 
//                                         TimeSpan.FromMilliseconds(Parameters.CONCEPTS_ACTIVATIONS_FREQUENCY), 
//                                         activeConceptProcessorRef, 
//                                         ProcessConcepts )
