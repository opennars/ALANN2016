module InferenceRuleHelpers

open Types
open Utilities

//
// Predicates for NAL
//
let measure_time(s, p) = false            // *** TODO complete this func
let notSet = function | IntSet _ | ExtSet _ -> false | _ -> true
let task(str, tv) = 
    match str with
    | "negative" when fst tv < 0.5f -> true
    | _ -> false
let set_int = function IntSet _ -> true | _ -> false
let set_ext = function ExtSet _ -> true | _ -> false
let not_set = function IntSet _ | ExtSet _ -> false | _ -> true
//let union(a, b, r) = true
let intersection(a, b, r) = true
let difference(a, b, r) = true
let shift_occurrence_forward(m, str) = true
let shift_occurrence_backward(m, str) = true
let after(t, b) = true
let not_implication_or_equivalence = function Imp _ | Equ _ -> true | _ -> false
let substitute_if_unifies(a, b, c) = true
let concurrent(a, b) = true
let not_conjunction = function And _ -> false | _ -> true
let belief = ""
let negative = ""
let substitute(a, b) = true
let sort lst = lst |> List.sort
let isDiff a b = not(Set.difference (set a) (set b) |> Set.isEmpty)
let diff a b = Set.difference (set a) (set b) |> Set.toList
let isIntersection a b = not(Set.intersect (set a) (set b) |> Set.isEmpty)
let intersect a b = Set.intersect (set a) (set b) |> Set.toList
let union a b = Set.union (set a) (set b) |> Set.toList
let commonSingleton a b = (Set.intersect (set a) (set b)) |> Set.count = 1
let removeCommon a b = ((set a) - (Set.intersect (set a) (set b)) |> Set.toList)
let occurrs term1 term2 =
    getTerms term2
    |> List.contains term1

let sub_ (ai : Term) lst =
    let i = lst |> List.findIndex (fun t -> t = ai)
    lst |> List.mapi (fun j t -> if j = i then Constant "_" else t )


