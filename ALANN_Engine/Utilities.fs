module Utilities

open Types

let sort lst = lst |> List.sort
let order(a, b) = if a < b then (a, b) else (b, a)
let contains t lst = Set.isProperSubset (Set [t]) (Set lst)
let removefrom lst t = lst |> List.filter (fun i -> i <> t)
let difference l1 l2 = Set.difference (set l1) (set l2) |> Set.toList
let not_imp_or_equ = function | Imp _ | Equ _ -> false | _ -> true
let listRemove t lst =
    let rec remove t lst =
        match lst with
        | [] -> []
        | x::rest when x = t -> rest
        | x::rest -> x::(remove t rest)
    let lst = remove t lst
    lst

let getSentence ((_, (term, _)), _) = term

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
    | _ -> failwith "Utilties.ActivePattern : Unexpected Term"

let rec syntacticComplexity term =
    match term with
    | ConstantTerm _ -> 1
    | UnaryTerm t -> 1 + syntacticComplexity t
    | BinaryTerm(s, p) -> 1 + syntacticComplexity s + syntacticComplexity p
    | ListTerm(lst) -> 1 + (lst |> List.fold (fun acc t -> acc + syntacticComplexity t) 0)
    | QueryTerm _ -> 2
    
let noCommonTerm(s, p) =
    
    let rec flatten acc term =
        let rec flattenList acc lst =
            match lst with
            | [] -> acc
            | x::rest -> (flatten acc x)@flattenList acc rest
        match term with
        | ConstantTerm(c) when c <> Constant "_" -> term::acc
        | UnaryTerm(s) -> term::(flatten acc s)
        | BinaryTerm(s, p) -> term::(flatten acc s @ flatten acc p)
        | ListTerm(lst) -> term::(flattenList acc lst)
        | QueryTerm(t) -> acc
        | _ -> acc
       
    not(Set.intersect (set (flatten [] s)) (set (flatten [] p)) |> Set.isEmpty)

let no_common_subterm = noCommonTerm

let (|LinkTerm|Other|) =
    function
    | Inh(s, p)    | Sim(s, p)    | Imp(s, p)
    | Equ(s, p)    | PreImp(s, p) | ConImp(s, p)
    | RetImp(s, p) | ConEqu(s, p) | PreEqu(s, p) -> LinkTerm(s, p)
    | _ -> Other

let getAllTerms term =
    match term with
    | LinkTerm(s, p) -> [s; p; term]
    | ListTerm(s) -> term::s
    | _ -> []

let depVar term =
    match term with
    | DVar _ -> true
    | _ -> false

let qVar term =
    match term with
    | QVar _ -> true
    | _ -> false

let noDepVar term = 
        match term with
        | LinkTerm(s, p) when depVar s || depVar p -> false
        | ListTerm(s) when s |> List.exists depVar -> false
        | _ -> true


let selective term = 
    term 
    |> getAllTerms
    |> List.exists qVar

let getTerms term =
    let rec flatten acc term =
        let rec flattenList acc lst =
            match lst with
            | [] -> acc
            | x::rest -> (flatten acc x)@flattenList acc rest
        match term with
        | ConstantTerm(c) when c <> Constant "_" -> term::acc
        | UnaryTerm(s) -> term::(flatten acc s)
        | BinaryTerm(s, p) -> term::(flatten acc s @ flatten acc p)
        | ListTerm(lst) -> term::(flattenList acc lst)
        | _ -> acc

    flatten [] term

let getTerms2 term =
    let rec flatten acc term =
        match term with
        | ConstantTerm(c) when c <> Constant "_" -> [term]
        | UnaryTerm(s) -> term::[s]
        | BinaryTerm(s, p) -> term::s::[p]
        | ListTerm(lst) -> term::lst
        | _ -> acc

    flatten [] term

let getTerms3 term =
    let rec flatten acc term =
        match term with
        | ConstantTerm(c) when c <> Constant "_" -> [term]
        | UnaryTerm(s) -> [term]
        | BinaryTerm(s, p) -> [s; p; term]
//        | And(lst) | Or(lst) -> term::lst
        | ListTerm(lst) -> [term]
        | _ -> acc

    flatten [] term

let isCommand task = task.Stamp.Origin = Origin.User && task.S.Key.SentenceType <> Judgement
