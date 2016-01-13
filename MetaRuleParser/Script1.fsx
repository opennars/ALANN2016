
#r @"D:\Visual Studio 2015\Projects\ALANN\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"D:\Visual Studio 2015\Projects\ALANN\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"
#r @"D:\Visual Studio 2015\Projects\ALANN\packages\Akka.FSharp.1.0.5\lib\net45\Akka.FSharp.dll"

open System.Collections.Generic

let equalsOn f x (yobj:obj) = 
        match yobj with
        | :? 'T as y -> (f x = f y)
        | _ -> false

let hashOn f x =  hash (f x)

let compareOn f x (yobj: obj) = 
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"

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

let (|ConstantTerm|UnaryTerm|BinaryTerm|ListTerm|QueryTerm|) (term : Term) =
    match term with
    | Constant str                                                          -> ConstantTerm(term)
    | Not s                                                                 -> UnaryTerm(s)
    | Inh(s, p)    | Sim(s, p)    | Imp(s, p) 
    | Equ(s, p)    | PreImp(s, p) | ConImp(s, p) 
    | RetImp(s, p) | ConEqu(s, p) | PreEqu(s, p)
    | ExtDif(s,p)  | IntDif(s, p)                                           -> BinaryTerm(s, p)
    | And lst      | Or lst       | Par lst
    | ExtSet lst   | IntSet lst   | ExtInt lst   | IntInt lst
    | Prod lst     | ExtImg lst   | IntImg lst
    | Seq lst      | Operator lst                                           -> ListTerm(lst)
    | QVar s       | DVar s       | IVar s                                  -> QueryTerm(term)

let rec syntacticComplexity term =
    match term with
    | ConstantTerm _ -> 1
    | UnaryTerm t -> 1 + syntacticComplexity t
    | BinaryTerm(s, p) -> 1 + syntacticComplexity s + syntacticComplexity p
    | ListTerm(lst) -> 1 + (lst |> List.fold (fun acc t -> acc + syntacticComplexity t) 0)
    | QueryTerm _ -> 2
    
//let noCommonTerm(s, p) =
//    
//    let traverse term =
//        match term with
//        | ConstantTerm(c) -> set [c; term]
//        | UnaryTerm(s) -> set [s; term]
//        | BinaryTerm(s, p) -> set [s; p; term]
//        | ListTerm(lst) -> set (term::lst)
//        | QueryTerm(t) -> set [t; term]
//
//    let sets = traverse s
//    let setp = traverse p
//       
//    Set.intersect sets setp |> Set.isEmpty

let noCommonTerm(s, p) =
    
    let rec flatten acc term =
        match term with
        | ConstantTerm(c) ->  term::acc
        | UnaryTerm(s) -> (flatten acc s)@term::acc
        | BinaryTerm(s, p) -> (flatten acc s)@(flatten acc s)@(term::acc)
        | ListTerm(lst) -> term::(List.fold flatten lst acc ) 
        | QueryTerm(t) -> term::acc

    let sets = flatten [] s
    let setp = flatten [] p
       
    not(Set.intersect (set sets) (set setp) |> Set.isEmpty)

let no_common_subterm = noCommonTerm

let (|LinkTerm|Other|) =
    function
    | Inh(s, p)    | Sim(s, p)    | Imp(s, p)
    | Equ(s, p)    | PreImp(s, p) | ConImp(s, p)
    | RetImp(s, p) | ConEqu(s, p) | PreEqu(s, p) -> LinkTerm(s, p)
    | _ -> Other

let term = Inh(ExtSet([Constant "John"]), ExtImg([Constant "taller_than"; ExtSet([Constant "Tom"; Constant "_"])]))

let getTaskTerms term =
    let rec flatten acc term =
        printfn "flatten term=%A" term
        match term with
        | ConstantTerm(c) when c <> Constant "_" ->  term::acc
        | UnaryTerm(s) -> term::(flatten acc s) 
        | BinaryTerm(s, p) -> term::(flatten acc s)@(flatten acc p)
        | ListTerm(lst) -> term::(List.fold flatten lst acc ) 
        | QueryTerm(t) -> acc
        | ConstantTerm(c) -> acc

    match term with
    | UnaryTerm(s) -> term::(flatten [] s)
//    | LinkTerm(IVar _, p) | LinkTerm(DVar _, p) | LinkTerm(QVar _, p) -> flatten [] p
//    | LinkTerm(s, IVar _) | LinkTerm(s, DVar _) | LinkTerm(s, QVar _) -> flatten [] s
    | LinkTerm(s, p) -> term::(flatten [] s)@(flatten [] p)
    | ListTerm(s) -> flatten s term
    | _ -> []

getTaskTerms term