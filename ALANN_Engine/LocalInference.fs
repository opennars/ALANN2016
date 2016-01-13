module LocalInference

open Akka.FSharp
open Types
open Trail
open Factory2
open TruthFunctions
open Messages
open Utilities
open QuestionUpdater

let beliefMatch = function
    | x, y when x = y -> true
    | Inh(QVar _, p1), Inh(_, p2) when p1 = p2 -> true
    | Sim(QVar _, p1), Sim(_, p2) when p1 = p2 -> true
    | Inh(s1, QVar _), Inh(s2, _) when s1 = s2 -> true
    | Sim(s1, QVar _), Sim(s2, _) when s1 = s2 -> true
    | LinkTerm(QVar _, p1), LinkTerm(_, p2) when p1 = p2 -> true
    | LinkTerm(s1, QVar _), LinkTerm(s2, _) when s1 = s2 -> true
    | LinkTerm(ExtSet([QVar _]), p1), LinkTerm(ExtSet([Constant _]), p2) when p1 = p2 -> true
    | LinkTerm(ExtSet([QVar _]), p1), LinkTerm(ExtSet([Constant _]), p2) when p1 = p2 -> true
    | LinkTerm(IntSet([QVar _]), p1), LinkTerm(IntSet([Constant _]), p2) when p1 = p2 -> true
    | LinkTerm(IntSet([QVar _]), p1), LinkTerm(IntSet([Constant _]), p2) when p1 = p2 -> true
    | _ -> false

let compareConf task belief =
    match task.S.BestAnswer with
    | Some answer 
        -> match answer.Sentence, belief.S.TV with
           | {TV = Some(f1, c1)}, Some(f2, c2) when c2 > c1 -> true
           | {TV = Some(f1, c1)}, Some(f2, c2) when c2 = c1 && f2 > f1 -> true
           | {TV = Some(f1, c1)}, Some(f2, c2) when c2 = c1 && f2 = f1 && belief.Stamp.SC < task.Stamp.SC -> true
           | _ -> false
    | None -> failwith "compareConf: expected answer"

let compareExp t b =
    match t.S.BestAnswer with
    | Some answer 
        -> match answer.Sentence, b.S.TV with
           | { TV = Some tv1 }, Some tv2 when exp(tv2)/single(b.Stamp.SC) > exp(tv1)/single(t.Stamp.SC) -> true
           | _ -> false
    | None -> failwith "compareConf: expected answer"

let checkBestAnswer state task belief =
    let updateBestAnswer task belief =
        let task = {task with S = {task.S with BestAnswer = Some {Sentence = belief.S; Stamp = belief.Stamp} } }
        if task.Stamp.Origin = Origin.User then
            questionUpdaterRef <! UpdateQuestions(task)
        belief.AV <- task.AV
        belief

    // is best answer so far?
    match task.S.BestAnswer with
    | None
        -> updateBestAnswer task belief

    | Some answer
        -> match task.IsSelective with
           | false -> if compareConf task belief then updateBestAnswer task belief else belief
           | true -> if compareExp task belief then updateBestAnswer task belief else belief

let getBestSelective lst =
    lst |> List.maxBy (fun b -> exp(b.S.TV.Value)/b.Stamp.SC)

let choice task state =
    let results =
        [for b in state.Beliefs -> b]
        |> List.filter (fun b -> beliefMatch (task.S.Key.Term, b.S.Key.Term))

    match results with
    | [] -> task
    | [b] when not task.IsSelective -> checkBestAnswer state task b
    | lst -> lst |> getBestSelective |> checkBestAnswer state task

let revision task (state : ConceptState) =
    let revise(t, b) =
        if nonOverlappingEvidence t.Stamp.Trail b.Stamp.Trail then
            makeBelief(t, b, b.S.Key.Term, Some(rev(task.S.TV.Value, b.S.TV.Value)), None, "Revision")
        else
             match t.S.TV, b.S.TV with
             | Some(f1, c1), Some(f2, c2) when c1 > c2 -> t
             | Some(f1, c1), Some(f2, c2) when c1 < c2 -> b
             | Some(f1, c1), Some(f2, c2) when exp(f1, c1) > exp(f2, c2) -> t
             | Some(f1, c1), Some(f2, c2) -> b
             | _ -> failwith "Revision.choice : unexpected None"

    if state.Beliefs.Contains task then
        state.Beliefs.Apply(revise, task)
    else
        task

let decision task state = task
