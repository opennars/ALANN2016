module Factory2
open Types
open Utilities
open Providers
open Parameters
open Trail
open AttentionFunctions

let makeTaskKey(sentenceType, term, occurs) = { SentenceType = sentenceType; Term = term; Occurs = occurs }

let makeSentence(taskKey, tv, dv) = { Key = taskKey; TV = tv; DV = dv; BestAnswer = None }

let makeNewStamp(id, occurs, term, origin) =
    { ID = id
      Created = SystemTime.Now
      Occurs = occurs
      SC = single(syntacticComplexity term)
      Origin = origin
      Trail = makeTrail0 id
      DerivationTrail = [term]
      DerivationRule = "Created"
      Activations = 0 } 

let makeNewTask s  =
    let p = 
        match s.Key.SentenceType with
        | Question -> Parameters.USERSTI
        | Judgement -> Parameters.USERSTI
        | _ -> Parameters.USERSTI

    { AV = p
      S = s
      Stamp = makeNewStamp( Id.Next(), SystemTime.Now, s.Key.Term, Origin.User ) 
      Router = []
      IsSelective = selective s.Key.Term
      Terms = getTerms s.Key.Term
    }

let blankTask = makeNewTask { Key = { SentenceType = Question; Term = Constant "Blank"; Occurs = 0L }; TV = None; DV = None; BestAnswer = None }

let tenseToOccurs tense =
    match tense with
    | Past -> SystemTime.Now - Parameters.DURATION
    | Present -> SystemTime.Now
    | Future -> SystemTime.Now + Parameters.DURATION
    | NoTense -> Eternal

let makeNewBelief s  =
    let p = 
        match s.Key.SentenceType with
        | Question -> Parameters.USERSTI
        | Judgement -> Parameters.USERSTI
        | _ -> Parameters.USERSTI

    { AV = p
      S = s
      Stamp = makeNewStamp( Id.Next(), s.Key.Occurs, s.Key.Term, Origin.User ) 
      Router = []
      IsSelective = false
      Terms = getTerms s.Key.Term
    }

let makeStamp(t, b, term, id, occurs, origin, derivationRule) =
    { ID = Id.Next()
      Created = SystemTime.Now 
      Occurs = occurs
      SC = single(syntacticComplexity term)
      Origin = origin
      Trail = makeTrail2 id t b
      DerivationTrail = makeDerivation3 t b term
      DerivationRule = derivationRule
      Activations = 0 }

let makeTask(t, b, term, tv, dv, derivationRule) = 
    { AV = b.AV * t.AV * 1.0f/t.Stamp.SC //t.AV
      S = { Key = {t.S.Key with Term = term; SentenceType = Question}; TV = tv; DV = dv; BestAnswer = None}
      Stamp = makeStamp( t, b, term, Id.Next(), SystemTime.Now, Origin.Derived, derivationRule) 
      Router = []
      IsSelective = selective t.S.Key.Term 
      Terms = getTerms term
    }

let makeBeliefStructuralStamp(t, b, term, id, occurs, origin, derivationRule) =
    { ID = Id.Next()
      Created = SystemTime.Now 
      Occurs = occurs
      SC = single(syntacticComplexity term)
      Origin = origin
      Trail = makeTrail2 id t b
      DerivationTrail = makeDerivation3 t b term
      DerivationRule = derivationRule
      Activations = 0 }

let makeBelief(t, b, term, tv, dv, derivationRule) = 

    { AV = b.AV * t.AV * snd(b.S.TV.Value) //b.AV
      S = { Key = {b.S.Key with Term = term; SentenceType = Judgement}; TV = tv; DV = dv; BestAnswer = None}
      Stamp = makeStamp( t, b, term, Id.Next(), SystemTime.Now, Origin.Derived, derivationRule) 
      Router = []
      IsSelective = false
      Terms = getTerms term }

let makeBelief2(t, b, occurs, term, tv, dv, derivationRule) = 

    { AV = b.AV
      S = { Key = {b.S.Key with Term = term; SentenceType = Judgement; Occurs = occurs}; TV = tv; DV = dv; BestAnswer = None}
      Stamp = makeStamp( t, b, term, Id.Next(), occurs, Origin.Derived, derivationRule) 
      Router = []
      IsSelective = false
      Terms = getTerms term }

let makeStructuralBelief(t, b, term, tv, dv, derivationRule) = 

    { AV = b.AV
      S = { Key = {b.S.Key with Term = term; SentenceType = Judgement}; TV = tv; DV = dv; BestAnswer = None}
      Stamp = makeBeliefStructuralStamp( t, b, term, Id.Next(), SystemTime.Now, Origin.Derived, derivationRule) 
      Router = []
      IsSelective = false 
      Terms = getTerms term}
