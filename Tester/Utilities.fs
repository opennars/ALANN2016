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
    printfn "%A" lst
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
        | BinaryTerm(s, p) -> (flatten acc s)@(flatten acc p)@(term::acc)
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

//let (|CompoundTerm|Other|) =
//    function
//    | ListTerm(s) -> CompoundTerm(s)
//    | _ -> Other

let getTerms term =
    match term with
    | LinkTerm(s, p) -> [s; p; term]
    | ListTerm(s) -> term::s
    | _ -> []

let getTaskTerms term =
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

    flatten [] term
//    let rec flatten acc term =
//        let rec flattenList acc lst =
//            match lst with
//            | [] -> acc
//            | x::rest -> (flatten acc x)@flattenList acc rest
//        match term with
//        | ConstantTerm(c) when c <> Constant "_" -> term::acc
//        | UnaryTerm(s) -> term::(flatten acc s)
////        | BinaryTerm(QueryTerm(_), p) -> flatten acc p
////        | BinaryTerm(s, QueryTerm(_)) -> flatten acc s
//        | BinaryTerm(s, p) -> term::(flatten acc s @ flatten acc p)
////        | ListTerm([QueryTerm(t)]) -> acc
//        | ListTerm(lst) -> term::(flattenList acc lst)
//        | QueryTerm(t) -> acc
//        | _ -> acc
//
//    flatten [] term
//    match term with
//    | UnaryTerm(s) -> term::(flatten [] s)
////    | LinkTerm(IVar _, p) | LinkTerm(DVar _, p) | LinkTerm(QVar _, p) -> flatten [] p
////    | LinkTerm(s, IVar _) | LinkTerm(s, DVar _) | LinkTerm(s, QVar _) -> flatten [] s
//    | LinkTerm(s, p) -> term::(flatten [] s)@(flatten [] p)
//    | ListTerm(s) -> term::(flatten s term)
//    | _ -> []

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
    |> getTerms
    |> List.exists qVar
