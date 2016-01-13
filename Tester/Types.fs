module Types

open System.Collections.Generic
open Helpers

type Origin         = | User        | Derived       | ParentModule  | PeerModule    | ChildModule
type Tense          = | Eternal     | Past          | Present       | Future        | NoTense

type TimeUnit       = int64
type Id             = int
type Trail          = Id list
type Routee         = int64

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
    | J of Term | Q of Term | G of Term | QQ of Term

type Truth          = (single * single)
type Desire         = (single * single)
type TruthType      = | GeneralTruth | StructuralTruth

type Stamp          = { ID : int
                        Created : TimeUnit
                        Occurs : TimeUnit
                        SC : int
                        Origin : Origin
                        Trail : Trail
                        DerivationTrail : Term list
                        DerivationRule : string
                        mutable Activations : int }

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
type Task           = { mutable AV : single
                        S : Sentence
                        Stamp : Stamp
                        Router : Router
                        IsSelective : bool }
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
    abstract RemoveMax : unit -> Task
    abstract Clear : unit -> unit
    abstract Count : int
    abstract GetEnumerator : unit -> IEnumerator<Task>

type ConceptRef     = { Term : Term; Ref : int64; mutable Activations : int }

type ConceptState = { Name : Term
                      Tasks: ITaskStore
                      Beliefs : ITaskStore
                      mutable Activation : single
                      mutable Priming : single
                      mutable LastActivation : TimeUnit
                      mutable LastUpdate : TimeUnit 
                      mutable Latent : bool
                      TaskBuffer : int64}

type MetricType = | DispatchCount | ActiveConceptCount

type Metric(name : MetricType, value : string) = 
    member x.Name with get () = name
    member x.Value with get () = value

type RuleFunc = (Term * Term) * (Truth * Truth) * (Truth * Truth) -> (Term * ((Truth * TruthType) option * (Desire * TruthType) option)) list
