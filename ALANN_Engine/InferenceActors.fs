module InferenceActors

open Types
open Akka.FSharp
open Akka.Actor
open ALANN
open Factory2
open EventFunctions
open Messages
open ForwardRules
open Trail
open Providers
open DerivedTaskBuffer

type InferenceRuleState = {Rule : RuleFunc; Name : string; mutable Logging : bool}

let getTruth task = 
    if task.S.TV |> Option.isSome 
    then task.S.TV.Value 
    else (0.0f, 0.0f)

let getDesire task = 
    if task.S.DV |> Option.isSome 
    then task.S.DV.Value 
    else (0.0f, 0.0f)

let checkDerivationTrail t b term =
    let tStr = TypeFormatters.TypeFormatter.ShortTask t
    let bStr = TypeFormatters.TypeFormatter.ShortTask b
    let bNonOverlap = nonOverlappingDerivations (term::t.Stamp.DerivationTrail) b.Stamp.DerivationTrail
    if not bNonOverlap then
        printfn "[%s], [%s] - nonOverlaping Derivation Trail %A" tStr bStr bNonOverlap

let getInfTerm task = 
    match task.S.Key.SentenceType with
    | Judgement -> J(task.S.Key.Term)
    | Question | Quest -> Q(task.S.Key.Term)
    | Goal -> G(task.S.Key.Term)

//Note: This type exists to resolve staic linking issues when building

type RuleBuilderForward() =
    do 
        let processResult task belief term tv dv tvType (dispatcher : ActorSelection) state = 
//            checkDerivationTrail task belief term
            let sentenceType = 
                match term with 
                | J(_)  -> SentenceType.Judgement
                | Q(_)  -> SentenceType.Question
                | QQ(_) -> SentenceType.Quest
                | G(_)  -> SentenceType.Goal
                | _ -> failwith "RuleBuilder.processResult: Unexpected term"

            let projectTV t1 t2 (f, c) =
                let d = 1.0f - single(t1 - t2) / single((SystemTime.Now - t1) + (SystemTime.Now  - t2))
                Some(f, c * d)

            let term = match term with | J(term) | Q(term) | G(term) -> term | _ -> failwith "RuleBuilder.processResult: Unexpected term"

            let result =
                match tvType with
                | GeneralTruth when task.S.Key.Occurs = Eternal && belief.S.Key.Occurs = Eternal
                    -> makeBelief2 (task, belief, Eternal, term, tv, dv, state.Name)
                | GeneralTruth when task.S.Key.Occurs <> Eternal && belief.S.Key.Occurs = Eternal
                    -> makeBelief2 (task, belief, task.S.Key.Occurs, term, tv, dv, state.Name)
                | GeneralTruth when task.S.Key.Occurs = Eternal && belief.S.Key.Occurs <> Eternal
                    -> makeBelief2 (task, belief, task.S.Key.Occurs, term, tv, dv, state.Name)
                | GeneralTruth when task.S.Key.Occurs <> Eternal && belief.S.Key.Occurs <> Eternal
                    -> makeBelief2 (task, belief, Eternal, term, projectTV task.S.Key.Occurs belief.S.Key.Occurs tv.Value, dv, state.Name)
                | StructuralTruth
                    -> makeStructuralBelief (task, belief, term, tv, dv, state.Name)
                | Backward
                    -> makeTask (task, belief, term, None, None, state.Name)

            if state.Logging then
                raiseInferenceResultEvent (Some task) (Some belief) [result]

            result |> (NewTask >> dispatcher.Tell)

        let createWorker (name, (f: RuleFunc )) = 
            let dispatcher = select "Akka://ALANN/user/taskBuffer" system
            let worker state (mailbox: Actor<SubscriberMessage>) msg = 
        
                let eventStream = mailbox.Context.System.EventStream
                
                match msg with
                | InferenceReq(task, belief) 
                    -> let tv1 = getTruth task
                       let tv2 = getTruth belief
                       let dv1 = getDesire task
                       let dv2 = getDesire belief
//                       let logmsg = sprintf "Doing Inference on: \n\tT:%s\n\tB:%s" (TypeFormatters.TypeFormatter.ShortTask task) (TypeFormatters.TypeFormatter.ShortTask belief)
//                       printfn "%s" logmsg
                       match state.Rule ((getInfTerm task, belief.S.Key.Term), (tv1, tv2), (dv1, dv2)) with
                       | [(term, (tvopt, dvopt))]
                           -> match tvopt, dvopt with
                              | Some (tv, tvType), None when snd tv > 0.0f 
                                -> processResult task belief term (Some tv) None tvType dispatcher state
                              | Some (tv, tvType), Some (dv, dvType) when snd tv > 0.0f 
                                -> processResult task belief term (Some tv) (Some dv) tvType dispatcher state
                              | None, Some (dv, dvType) when snd dv > 0.0f
                                -> processResult task belief term None (Some dv) dvType dispatcher state
                              | None, None 
                                -> processResult task belief term None None TruthType.Backward dispatcher state
                              | _ -> ()
                       | _ -> ()

                | SubscribeToInferenceReq -> subscribe typeof<SubscriberMessage> mailbox.Self eventStream |> ignore
                | EnableInferenceLogging -> state.Logging <- true
                | DisableInferenceLogging -> state.Logging <- false
        
            spawn system name (actorOf2 (worker({Rule = f; Name = name; Logging = false})))

        let createWorkers prefix rules =
            let rec createWorkerRec (i, refs) rules =
                match rules with
                | [] -> refs
                | rule::rules 
                    -> let refs = createWorker (prefix + "Rule" + i.ToString(), rule)::refs
                       createWorkerRec (i + 1, refs) rules

            createWorkerRec (1, []) rules

        createWorkers "Forward" forwardRules |> List.iter (fun ref -> ref <! SubscribeToInferenceReq)
