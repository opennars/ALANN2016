module BackwardInferenceActor

open Types
open Akka.FSharp
open ALANN
open Factories
open EventFunctions
open Messages
open BackwardRules
open TaskRules

let getTruth task = 
    if task.S.TV |> Option.isSome 
    then task.S.TV.Value 
    else (0.0f, 0.0f)

//Note: This type exists to resolve staic linking issues when building

type RuleBuilderBackward() =
    do 
        let createBackwardWorker (name, (f: RuleFunc )) = 
            let dispatcher = select "Akka://ALANN/user/taskBuffer" system
            let worker (mailbox: Actor<BackwardInferenceRuleMsg>) msg = 
        
                let eventStream = mailbox.Context.System.EventStream
                
                match msg with
                | BackwardInferenceRuleReq(task, belief) 
                    -> let tv1 = getTruth task
                       let tv2 = getTruth belief

                       match f (task, belief) (tv1, tv2) with
                       | [(term, tvopt, dvopt)] 
                           -> let result = makeTask3 (task, belief, term, None, None, name)
                              raiseInferenceResultEvent (Some task) (Some belief) [result]
                              result |> (TaskMsg >> dispatcher.Tell)
                       | _ -> ()

                | SubscribeToBackward -> subscribe typeof<BackwardInferenceRuleMsg> mailbox.Self eventStream |> ignore
        
            spawn system name (actorOf2 worker)

        let createWorkers prefix rules =
            let rec createWorkerRec (i, refs) rules =
                match rules with
                | [] -> refs
                | rule::rules 
                    -> let refs = createBackwardWorker (prefix + "Rule" + i.ToString(), rule)::refs
                       createWorkerRec (i + 1, refs) rules

            createWorkerRec (1, []) rules

        createWorkers "Backward"  backwardRules |> List.iter (fun ref -> ref <! SubscribeToBackward)
        createWorkers "BackwardTask" backwardTaskRules |> List.iter (fun ref -> ref <! SubscribeToBackward)
