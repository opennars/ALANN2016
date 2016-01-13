module Factories

open System
open Types
open Providers
open Trail
open Parameters
open Utilities
open Akka.FSharp
//open ALANN
open AttentionFunctions

let makeStamp(id, occurs, term, origin) =
    { ID = id
      Created = SystemTime.Now
      Occurs = occurs
      SC = syntacticComplexity term
      Origin = origin
      Trail = makeTrail0 id
      DerivationTrail = [term]
      DerivationRule = "Created"
      Activations = 0 } 

//let makeStamp2(task, belief, occurs, sc, origin) =
//    { Created = SystemTime.Now 
//      Occurs = occurs
//      SC = sc
//      Origin = origin
//      Trail = makeTrail2 task belief
//      DerivationTrail = makeDerivation2 task belief
//      LastActivated = SystemTime.Now }

let makeStamp3(task, belief, term, id, occurs, origin, derivationRule) =
    { ID = Id.Next()
      Created = SystemTime.Now 
      Occurs = occurs
      SC = syntacticComplexity term
      Origin = origin
      Trail = makeTrail2 id task belief
      DerivationTrail = makeDerivation3 task belief term
      DerivationRule = derivationRule
      Activations = 0 }
      
let makeTaskKey(sentenceType, term, tense) = { SentenceType = sentenceType; Term = term; Tense = tense }

let makeSentence(taskKey, tv, dv) = { Key = taskKey; TV = tv; DV = dv; BestAnswer = None }

let makeTask3(parentTask, parentBelief, term, tv, dv, derivationRule) = 

    { AV = parentBelief.AV
      S = { Key = {parentTask.S.Key with Term = term}; TV = tv; DV = dv; BestAnswer = None}
      Stamp = makeStamp3( parentTask, 
                          parentBelief, 
                          term,
                          Id.Next(),
                          SystemTime.Now, 
                          Origin.Derived,
                          derivationRule) 
      Router = []
      IsSelective = selective parentTask.S.Key.Term }

let makeTask4(parentTask, parentBelief, term, sentenceType, tv, dv, derivationRule) = 

    { AV = parentBelief.AV
      S = { Key = {parentTask.S.Key with Term = term; SentenceType = sentenceType}; TV = tv; DV = dv; BestAnswer = None}
      Stamp = makeStamp3( parentTask, 
                          parentBelief, 
                          term,
                          Id.Next(),
                          SystemTime.Now, 
                          Origin.Derived,
                          derivationRule) 
      Router = []
      IsSelective = selective parentTask.S.Key.Term }


let makeTask3NoStampMerge(t, b, term, tv, dv, derivationRule) = 

    { AV = b.AV
      S = { Key = {t.S.Key with Term = term}; TV = tv; DV = dv; BestAnswer = None}
      Stamp = { t.Stamp with 
                    Trail = makeTrail2 t.Stamp.ID t b
                    DerivationTrail = makeDerivation3 t b term
                    DerivationRule = derivationRule}
      Router = []
      IsSelective = selective t.S.Key.Term }

let makeTask4NoStampMerge(t, b, term, sentenceType, tv, dv, derivationRule) = 

    { AV = b.AV
      S = { Key = {t.S.Key with Term = term; SentenceType = sentenceType}; TV = tv; DV = dv; BestAnswer = None}
      Stamp = { t.Stamp with 
                    Trail = makeTrail2 t.Stamp.ID t b
                    DerivationTrail = makeDerivation3 t b term
                    DerivationRule = derivationRule}
      Router = []
      IsSelective = selective t.S.Key.Term }

let makeNewTask s  =
    let p = 
        match s.Key.SentenceType with
        | Question -> Parameters.USERSTI
        | Judgement -> Parameters.STI
        | _ -> Parameters.STI

    { AV = Parameters.USERLTI
      S = s
      Stamp = makeStamp( Id.Next(), SystemTime.Now, s.Key.Term, Origin.User ) 
      Router = []
      IsSelective = selective s.Key.Term
    }

