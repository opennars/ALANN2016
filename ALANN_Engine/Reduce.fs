module Reduce

open Types

//let (|IntSetPat|ExtSetPat|IntIntPat|ExtIntPat|OrPat|AndPat|Other|) (term : Term) =
//    match term with
//    | IntSet set -> IntSetPat(set |> Set.toList)
//    | ExtSet set -> ExtSetPat(set |> Set.toList)
//    | IntInt set -> IntIntPat(set |> Set.toList)
//    | ExtInt set -> ExtIntPat(set |> Set.toList)
//    | Or set -> OrPat(set |> Set.toList)
//    | And set -> AndPat(set |> Set.toList)
//    | _ -> Other
//
//let rec reduce (st : Term) =
//    match st with
//    | ExtIntPat([t]) -> t
//    | IntIntPat([t]) -> t
//    | ExtIntPat([ExtInt(l1); ExtInt(l2)]) -> ExtInt(Set.union l1 l2)
//    | ExtIntPat([ExtInt(l1); l2]) -> ExtInt(Set.union l1 (set [l2]))
//    | ExtIntPat([l1; ExtInt(l2)]) -> ExtInt(Set.union (set [l1]) l2)
//    | ExtIntPat([IntSet(l1); IntSet(l2)]) -> IntSet(Set.union l1 l2)
//    | IntIntPat([IntInt(l1); IntInt(l2)]) -> IntInt(Set.union l1 l2)
//    | IntIntPat([IntInt(l1); l2]) -> IntInt(Set.union l1 (set [l2]))
//    | IntIntPat([l1; IntInt(l2)]) -> IntInt(Set.union (set [l1]) l2)
//    | IntIntPat([IntSet(l1); IntSet(l2)]) -> IntSet(Set.union l1 l2)
//    | IntIntPat([ExtSet(l1); ExtSet(l2)]) -> ExtSet(Set.union l1 l2)
//    | ExtDif(ExtSet(l1), ExtSet(l2)) -> ExtSet(Set.difference l1 l2)
//    | IntDif(IntSet(l1), IntSet(l2)) -> IntSet(Set.difference l1 l2)
//    | Sim(ExtSetPat([s]), ExtSetPat([p])) -> Sim(s, p)
//    | Sim(IntSetPat([s]), IntSetPat([p])) -> Sim(s, p)
//    | Prod([Prod(lst); t]) -> Prod(lst @ [t])
//    | ExtImg([Prod([t1; t2]); t3]) when t2 = t3 && t1 <> t2 -> t1
//    | IntImg([Prod([t1; t2]); t3]) when t2 = t3 && t1 <> t2 -> t1
//    | Not(Not(s)) -> reduce s
//    | AndPat([t]) -> reduce t
//    | OrPat([t]) -> reduce t
//    | OrPat([t1; t2]) when t1 = t2 -> reduce t2
//    | AndPat([t1; t2]) when t1 = t2 -> reduce t1
//    | AndPat([AndPat(l1); AndPat(l2)]) -> reduce(And(Set.union (set l1)(set l2)))
//    | AndPat([AndPat(l1); l2]) -> reduce(And(Set.union (set l1)(set [l2])))
//    | AndPat([l1; AndPat(l2)]) -> reduce(And(Set.union (set [l1])(set l2)))
//    | OrPat([OrPat(l1); OrPat(l2)]) -> reduce(Or(Set.union (set l1)(set l2)))
//    | OrPat([OrPat(l1); l2]) -> reduce(Or(Set.union (set l1)(set [l2])))
//    | OrPat([l1; Or(l2)]) -> reduce(Or(Set.union (set [l1])(set l2)))
//    | _ -> st

let rec reduce (st : Term) =
    match st with
    | ExtInt([t]) -> t
    | IntInt([t]) -> t
    | ExtInt([ExtInt(l1); ExtInt(l2)]) -> ExtInt(Set.union (set l1) (set l2) |> Set.toList)
    | ExtInt([ExtInt(l1); l2]) -> ExtInt(Set.union (set l1) (set [l2]) |> Set.toList)
    | ExtInt([l1; ExtInt(l2)]) -> ExtInt(Set.union (set [l1]) (set l2) |> Set.toList)
    | ExtInt([IntSet(l1); IntSet(l2)]) -> IntSet(Set.union (set l1) (set l2) |> Set.toList)
    | IntInt([IntInt(l1); IntInt(l2)]) -> IntInt(Set.union (set l1) (set l2) |> Set.toList)
    | IntInt([IntInt(l1); l2]) -> IntInt(Set.union (set l1) (set [l2]) |> Set.toList)
    | IntInt([l1; IntInt(l2)]) -> IntInt(Set.union (set [l1]) (set l2) |> Set.toList)
    | IntInt([IntSet(l1); IntSet(l2)]) -> IntSet(Set.union (set l1) (set l2) |> Set.toList)
    | IntInt([ExtSet(l1); ExtSet(l2)]) -> ExtSet(Set.union (set l1) (set l2) |> Set.toList)
    | ExtDif(ExtSet(l1), ExtSet(l2)) -> ExtSet(Set.difference (set l1) (set l2) |> Set.toList)
    | IntDif(IntSet(l1), IntSet(l2)) -> IntSet(Set.difference (set l1) (set l2) |> Set.toList)
    | Sim(ExtSet([s]), ExtSet([p])) -> Sim(s, p)
    | Sim(IntSet([s]), IntSet([p])) -> Sim(s, p)
    | Prod([Prod(lst); t]) -> Prod(lst @ [t])
    | ExtImg([Prod([t1; t2]); t3]) when t2 = t3 && t1 <> t2 -> t1
    | IntImg([Prod([t1; t2]); t3]) when t2 = t3 && t1 <> t2 -> t1
    | Not(Not(s)) -> reduce s
    | And([t]) -> reduce t
    | Or([t]) -> reduce t
    | Or([t1; t2]) when t1 = t2 -> reduce t2
    | And([t1; t2]) when t1 = t2 -> reduce t1
    | And([And(l1); And(l2)]) -> reduce(And(Set.union (set l1)(set l2) |> Set.toList))
    | And([And(l1); l2]) -> reduce(And(Set.union (set l1)(set [l2]) |> Set.toList))
    | And([l1; And(l2)]) -> reduce(And(Set.union (set [l1])(set l2) |> Set.toList))
    | Or([Or(l1); Or(l2)]) -> reduce(Or(Set.union (set l1)(set l2) |> Set.toList))
    | Or([Or(l1); l2]) -> reduce(Or(Set.union (set l1)(set [l2]) |> Set.toList))
    | Or([l1; Or(l2)]) -> reduce(Or(Set.union (set [l1])(set l2) |> Set.toList))
    | _ -> st


