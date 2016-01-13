module TaskDispatcherFunctions

open Akka.FSharp
open Types
open Parameters
open Concept
open Messages
open ConceptStore
open Utilities

type TaskDispatcherState = {Concepts : ConceptStore; mutable Id : int; mutable Count : int; Threshold : single}

let getConcepts (dict : ConceptStore) =
    [|for c in dict -> c|]
    |> Array.map (fun v -> Async.RunSynchronously(v.Ref <? GetConceptState))
    |> Array.sortBy (fun c -> -c.Activation)
    |> Array.take (min dict.Count 500)

let getRoutees t state =
    let termToRef state term =
        match state.Concepts.TryGetValue(term) with
        | Some c 
            -> c.Ref

        | None _ 
            -> let c = makeConcept state.Id term
               state.Id <- state.Id + 1
               state.Concepts.Insert(c) |> Option.iter (fun c -> c.Ref <! Stop)
               c.Ref

//    let refs = t.Terms |> List.map (termToRef state)
//    let refTerms = List.zip refs t.Terms
//
//    printfn "Routees for: %s" (TypeFormatters.TypeFormatter.Term t.S.Key.Term)
//    refTerms |> List.iter (fun (ref, term) -> printfn "\t%s : %s" (ref.ToString()) (TypeFormatters.TypeFormatter.Term term))

    t.Terms
//    getTerms2 t.S.Key.Term
    |> List.map (termToRef state)

let dispatchTaskToTerms state t =
    let routees = getRoutees t state
    routees |> List.iter 
        (fun r -> r <! NewTask({t with Router = routees}) )

let newTaskDispatcherState() = 
    { Concepts = ConceptStore(Parameters.CONCEPT_CAPACITY)
      Id = 0
      Count = 0
      Threshold = Parameters.ACTIVATION_THRESHOLD }

