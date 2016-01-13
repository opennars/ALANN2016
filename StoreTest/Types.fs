module Types

open System.Collections.Generic
open IStore

type Origin         = | User        | Derived       | ParentModule  | PeerModule    | ChildModule
type Tense          = | Eternal     | Past          | Present       | Future        | NoTense

type TimeUnit       = int64
type Id             = int64
type Trail          = Id list
type Routee         = int64

type Term =
    | Constant of string
    | Inh of Term * Term                                                                                // NAL 1
    | Sim of Term * Term      | ExtSet of Term Set     | IntSet of Term Set                             // NAL 2
    | ExtInt of Term Set      | IntInt  of Term Set    | ExtDif of Term * Term | IntDif of Term * Term  // NAL 3
    | Prod of Term list       | ExtImg of Term list    | IntImg of Term list                            // NAL 4
    | Not of Term             | And of Term Set        | Or of Term Set                                 // NAL 5
    | Imp of Term * Term      | Equ of Term * Term                                                      // NAL 5
    | QVar of string          | DVar of string         | IVar of string                                 // NAL 6
    | PreImp of Term * Term   | ConImp of Term * Term  | RetImp of Term * Term                          // NAL 7
    | ConEqu of Term * Term   | PreEqu of Term * Term                                                   // NAL 7
    | Par of Term Set         | Seq of Term list                                                        // NAL 7
    | Operator of Term list                                                                             // NAL 8

type Budget         = { mutable P : single; D : single }
type Truth          = (single * single)
type Desire         = (single * single)

type Stamp          = { Created : TimeUnit
                        Occurs : TimeUnit
                        SC : int
                        Origin : Origin
                        Trail : Trail
                        Activations : int }

type SentenceType   = | Judgement | Question | Quest | Goal

type TaskKey        = { SentenceType : SentenceType
                        Term : Term
                        Tense : Tense }


type Sentence       = { Key : TaskKey
                        TV : Truth option
                        DV : Desire option
                        BestAnswer : Answer option}

and Answer          = { Sentence : Sentence 
                        Stamp : Stamp }

type Router         = Routee list

open Helpers
[<CustomEquality; CustomComparison>]
type Task           = { AV : Budget; S : Sentence; Stamp : Stamp; Router : Router}
                        interface IStorable<TaskKey> with
                            member this.Key = this.S.Key
                            member this.P
                                with get (): single = this.AV.P and set(v) = this.AV.P <- v

                        static member Key(task) = task.S.Key
                        override x.Equals y = equalsOn Task.Key x y
                        override x.GetHashCode() = hashOn Task.Key x
                        interface System.IComparable with
                            member x.CompareTo y = compareOn Task.Key x y
//                  
//  Sample useage:-
//
//  type stamp = int
//
//  [<CustomEquality; CustomComparison>]
//
//  type MyUnionType = 
//      | MyUnionType of stamp * (int -> int)
//
//      static member Stamp (MyUnionType (s,_)) = s
//      override x.Equals y = equalsOn MyUnionType.Stamp x y
//      override x.GetHashCode() = hashOn MyUnionType.Stamp x
//      interface System.IComparable with
//        member x.CompareTo y = compareOn MyUnionType.Stamp x y

//type Concept        = { Name : Term
//                        Tasks: IStore<TaskKey, Task>
//                        Beliefs : IStore<TaskKey, Task>
//                        mutable Activation : single
//                        mutable LastActivation : TimeUnit
//                        mutable LastUpdate : TimeUnit }
//                        interface IStorable<Term> with
//                            member this.Key = this.Name
//                            member this.P = 1.0f

type ConceptRef     = { Term : Term; Ref : int64; mutable P : single }
                        interface IStorable<Term> with
                            member this.Key = this.Term
                            member this.P
                                with get (): single = this.P and set(v) = this.P <- v

type InputBuffer    = IBuffer<Task>
type TaskBuffer     = IBuffer<Task>
//type ActiveConcepts = IBuffer<Concept>
//type ConceptStore   = IStore<Term, Concept>

type ConceptState = { Name : Term
                      Tasks: IStore<TaskKey, Task>
                      Beliefs : IStore<TaskKey, Task>
                      mutable Actives : Set<Task>
                      mutable Activation : single
                      mutable SpikeActivation : single
                      mutable LastActivation : TimeUnit
                      mutable LastUpdate : TimeUnit 
                      TaskDispatcher : int64}

type Metric(name : string, value : string) = 
    member x.Name with get () = name
    member x.Value with get () = value

type RuleFunc = (Task * Task) -> (Truth * Truth) -> (Term * Truth option * Desire option) list
