module Types

open System.Collections.Generic
open Helpers

type IActorRef      = int64
type ActorSelection = int64

type Origin         = | User        | Derived       | ParentModule  | PeerModule    | ChildModule
type Tense          = | Eternal     | Past          | Present       | Future        | NoTense

type TimeUnit       = int64
type Id             = int64
type Trail          = Id list
type Routee         = IActorRef

type Term =
    | Constant of string
    | Inh of Term * Term                                                                                 // NAL 1
    | Sim of Term * Term      | ExtSet of Term list     | IntSet of Term list                            // NAL 2
    | ExtInt of Term list     | IntInt  of Term list    | ExtDif of Term * Term | IntDif of Term * Term  // NAL 3
    | Prod of Term list       | ExtImg of Term list     | IntImg of Term list                            // NAL 4
    | Not of Term             | And of Term list        | Or of Term list                                // NAL 5
    | Imp of Term * Term      | Equ of Term * Term                                                       // NAL 5
    | QVar of string          | DVar of string         | IVar of string                                  // NAL 6
    | PreImp of Term * Term   | ConImp of Term * Term  | RetImp of Term * Term                           // NAL 7
    | ConEqu of Term * Term   | PreEqu of Term * Term                                                    // NAL 7
    | Par of Term list        | Seq of Term list                                                         // NAL 7
    | Operator of Term list                                                                              // NAL 8

type Budget         = { mutable P : single; D : single }
type Truth          = (single * single)
type Desire         = (single * single)

type Stamp          = { Created : TimeUnit
                        Occurs : TimeUnit
                        SC : int
                        Origin : Origin
                        Trail : Trail
                        LastActivated : TimeUnit }

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

[<CustomEquality; CustomComparison>]
type Task           = { AV : Budget; S : Sentence; Stamp : Stamp; Router : Router}
                        static member Key(task) = task.S.Key
                        override x.Equals y = equalsOn Task.Key x y
                        override x.GetHashCode() = hashOn Task.Key x
                        interface System.IComparable with
                            member x.CompareTo y = compareOn Task.Key x y

and ITaskStore =
    abstract Contains : Task -> bool
    abstract Insert : Task -> unit
    abstract Update : Task -> unit
    abstract Merge : Task -> unit
    abstract Apply : (Task * Task -> Task) * Task -> Task
    abstract TryGetValue : Task -> Task option
    abstract Clear : unit -> unit
    abstract Count : int
    abstract GetEnumerator : unit -> IEnumerator<Task>

type ConceptRef     = { Term : Term; Ref : IActorRef; mutable P : single }

type ConceptState = { Name : Term
                      Tasks: ITaskStore
                      Beliefs : ITaskStore
                      mutable Actives : Set<Task>
                      mutable Activation : single
                      mutable SpikeActivation : single
                      mutable LastActivation : TimeUnit
                      mutable LastUpdate : TimeUnit 
                      TaskDispatcher : ActorSelection}

type Metric(name : string, value : string) = 
    member x.Name with get () = name
    member x.Value with get () = value

type RuleFunc = (Task * Task) -> (Truth * Truth) -> (Term * Truth option * Desire option) list
