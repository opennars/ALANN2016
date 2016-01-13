module ConceptFunctions

open System
open ALANN
open Akka.FSharp
open Types
open Trail
open Messages
open LocalInference
open Parameters
open Utilities
open Providers

let decayPrimer state (belief : Task) =
    // Decay activation level by e^-ld
    let now = SystemTime.Now
    let delta = now - state.LastUpdate
    belief.AV <- belief.AV * Operators.exp(-Parameters.DECAY_RATE*single(delta))
    belief

let decayBeliefs state =
    if state.Priming <> 0.0f then
        let priming = 
            [for b in state.Beliefs -> b]
            |> List.sumBy (fun b -> state.Beliefs.Update(decayPrimer state b); b.AV)
        state.Priming <- min priming 0.5f

let beliefAbovePrimingThreshold belief =
    belief.AV >= Parameters.PRIME_THRESHOLD

let updateActiveBeliefs belief primer state =
    let terms = belief.Terms
    let beliefs = [for b in state.Beliefs -> b]

    let termMatch term b =
        terms |> List.exists (fun t -> b.Terms |> List.contains t)

    let matches =
        beliefs |> List.filter (termMatch terms)            

    matches 
    |> List.iter(fun belief ->
            belief.Stamp.Activations <- belief.Stamp.Activations + 1
            state.Beliefs.Update(belief)
    )

//    match state.Beliefs.Contains belief with
//    | true -> 
////        belief.AV <- min ((primer + (decayPrimer state belief).AV) / 2.0f) 1.0f
//        belief.Stamp.Activations <- belief.Stamp.Activations + 1
//        state.Beliefs.Update(belief)
//    | false -> ()

let decayActivation state =
    // Decay activation level by e^-ld
    let now = SystemTime.Now
    let delta = now - state.LastUpdate
    state.Activation <- state.Activation * Operators.exp(-Parameters.DECAY_RATE*single(delta))
    state.LastUpdate <- now

let updateActivation task state =
//    decayActivation state
    state.Activation <- min (state.Priming + task.AV) 0.5f

let updatePriming belief state =
//    decayActivation state
    state.Priming <- min (state.Priming + belief.AV) 0.5f

let handlePrimer belief spike state =
     updatePriming belief state
     updateActiveBeliefs belief spike state

let localInference task (state : ConceptState) =
    let isRevisable t = 
        t.Stamp.Occurs > (SystemTime.Now - Parameters.DURATION) && 
        t.Stamp.Occurs < (SystemTime.Now + Parameters.DURATION) &&
        noDepVar t.S.Key.Term

    match task.S.Key.SentenceType with
    | Judgement when isRevisable task -> revision task state
    | Question -> choice task state
    | Goal -> decision task state
    | _ -> task

let insertTask state task =
    match state.Tasks.Contains task with
    | false -> state.Tasks.Insert task
    | true -> state.Tasks.Merge task
    match task.S.Key.SentenceType with
    | Judgement -> 
        match state.Beliefs.Contains task with
        | false -> state.Beliefs.Insert task |> ignore
        | true -> state.Beliefs.Update(task)
    | _ -> ()
        
let aboveInferenceThreshold t b =
    let threshold = Parameters.INFERENCE_THRESHOLD

    match t.S.Key.SentenceType with
    | Judgement when snd t.S.TV.Value > threshold && snd b.S.TV.Value > threshold -> true
    | Question when snd b.S.TV.Value > threshold -> true
    | _ -> false

let resetActivation state =
    state.Active <- false
    state.Activation <- 0.0f
    state.Priming <- 0.0f
    state.LastActivation <- SystemTime.Now
    state.LastUpdate <- SystemTime.Now

let getActiveBeliefs state =
    let actives =
        [|for b in state.Beliefs -> b|]
        |> Array.sortBy (fun b -> -b.AV)
        |> Array.filter beliefAbovePrimingThreshold

//    let logmsg = 
//        sprintf "Start:\n\t%s"
//            (actives 
//            |> List.fold 
//                (fun acc b -> 
//                    acc + (sprintf("Active: %s\n") (TypeFormatters.TypeFormatter.ShortTask b))) "")
//    printfn "%s" logmsg
    actives

let doInference state (mailbox: Actor<'a>) actives =
    let isValidInference t b = 
        aboveInferenceThreshold t b &&
        nonOverlappingEvidence t.Stamp.Trail b.Stamp.Trail &&
        nonOverlappingDerivations t.Stamp.DerivationTrail b.Stamp.DerivationTrail

    let inferenceReq t b = publish (InferenceReq(t, b)) mailbox.Context.System.EventStream |> ignore

    let tasks = [|for t in state.Tasks -> t|]
    let beliefs = actives |> Array.take (min (actives |> Array.length) Parameters.INFERENCE_BELIEFS_PER_CYCLE)
    for j in 0..tasks.Length - 1 do
        for k in 0..beliefs.Length - 1 do
            let t = tasks.[j]
            let b = beliefs.[k]
            if isValidInference t b then 
                inferenceReq t b


//    let tasks = [|for t in state.Tasks -> t|]
//    for j in 0..tasks.Length - 1 do
//        for k in 0..beliefs.Length - 1 do
//            let t = tasks.[j]
//            let b = beliefs.[k]
//            if isValidInference t b then 
//                publish (ForwardInferenceRuleReq(t, b)) mailbox.Context.System.EventStream |> ignore
    
let prime ref state actives =
    let notSelf r = r <> ref
    let primeReq b r = r <! Primer(b, (min b.AV 1.0f) * Parameters.PRIME_ACTIVATION)

    // Prime all beliefs in actives
    let routePrimer b =
        b.Router
        |> List.filter notSelf
        |> List.iter (primeReq b)

    actives
    |> Array.take (min (actives |> Array.length) Parameters.PRIME_ACTIVATIONS_MAX)
    |> Array.append [|for t in state.Tasks -> t|]
    |> Array.iter routePrimer

let canActivate state = not(state.Active) && (state.Priming + state.Activation) >= Parameters.ACTIVATION_THRESHOLD
    
let activate state mailbox =
    let actives = getActiveBeliefs state
    doInference state mailbox actives
    prime mailbox.Self state actives
    resetActivation state
