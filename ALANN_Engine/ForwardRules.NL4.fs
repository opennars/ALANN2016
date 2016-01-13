
module ForwardRules

open System
open Types
open TruthFunctions
open Utilities
open Reduce
open Substitution
open InferenceRuleHelpers

let forwardRules : RuleFunc list =
    [

// Rule 0


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(s1,p1)), Sim(s2,p2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(J(Inh(s1, p1)), (Some(structuralInt(tv1)), None))]
           | _ -> []
       )

// Rule 1


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(s1,p1)), Inh(s2,p2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(J(Sim(s1, p1)), (Some(structuralAbd(tv1)), None))]
           | _ -> []
       )

// Rule 2


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(J(Inh(ExtSet([s]), ExtSet([p]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 3


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(Q(Inh(ExtSet([s]), ExtSet([p]))), (None, None))]
           | _ -> []
       )

// Rule 4


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(J(Inh(ExtSet([s]), ExtSet([p]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 5


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(Q(Inh(ExtSet([s]), ExtSet([p]))), (None, None))]
           | _ -> []
       )

// Rule 6


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(J(Inh(IntSet([s]), IntSet([p]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 7


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(Q(Inh(IntSet([s]), IntSet([p]))), (None, None))]
           | _ -> []
       )

// Rule 8


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(J(Inh(IntSet([s]), IntSet([p]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 9


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(Q(Inh(IntSet([s]), IntSet([p]))), (None, None))]
           | _ -> []
       )

// Rule 10


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(J(Inh(ExtSet([p]), ExtSet([s]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 11


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(Q(Inh(ExtSet([p]), ExtSet([s]))), (None, None))]
           | _ -> []
       )

// Rule 12


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(J(Inh(ExtSet([p]), ExtSet([s]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 13


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(Q(Inh(ExtSet([p]), ExtSet([s]))), (None, None))]
           | _ -> []
       )

// Rule 14


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(J(Inh(IntSet([p]), IntSet([s]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 15


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(Q(Inh(IntSet([p]), IntSet([s]))), (None, None))]
           | _ -> []
       )

// Rule 16


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(J(Inh(IntSet([p]), IntSet([s]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 17


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(Q(Inh(IntSet([p]), IntSet([s]))), (None, None))]
           | _ -> []
       )

// Rule 18


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(J(Sim(s, p)), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 19


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 20


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(J(Sim(s, p)), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 21


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(ExtSet([s]), ExtSet([p]))), _ when s <> p -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 22


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(J(Sim(s, p)), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 23


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 24


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(J(Sim(s, p)), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 25


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(IntSet([s]), IntSet([p]))), _ when s <> p -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 26


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, ExtSet([p]))), _ when s <> p -> [(J(Sim(s, ExtSet([p]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 27


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(s, ExtSet([p]))), _ when s <> p -> [(Q(Sim(s, ExtSet([p]))), (None, None))]
           | _ -> []
       )

// Rule 28


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, ExtSet([p]))), _ when s <> p -> [(J(Sim(s, ExtSet([p]))), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 29


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(s, ExtSet([p]))), _ when s <> p -> [(Q(Sim(s, ExtSet([p]))), (None, None))]
           | _ -> []
       )

// Rule 30


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(IntSet([s]), p)), _ when s <> p -> [(J(Sim(IntSet([s]), p)), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 31


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntSet([s]), p)), _ when s <> p -> [(Q(Sim(IntSet([s]), p)), (None, None))]
           | _ -> []
       )

// Rule 32


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(IntSet([s]), p)), _ when s <> p -> [(J(Sim(IntSet([s]), p)), (Some(identity(tv1)), Some(identity(dv1))))]
           | _ -> []
       )

// Rule 33


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntSet([s]), p)), _ when s <> p -> [(Q(Sim(IntSet([s]), p)), (None, None))]
           | _ -> []
       )

// Rule 34


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(p1, s1)), Inh(s2, p2) when s1 <> p1 && s1 = s2 && p1 = p2-> [(J(Inh(p1, s1)), (Some(cnv(tv2)), None))]
           | _ -> []
       )

// Rule 35


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, b)), _ when task(negative, tv1) -> [(J(Not(Inh(a, b))), (Some(neg(tv1)), Some(desireNeg(dv1))))]
           | _ -> []
       )

// Rule 36


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Not(Inh(a, b))), _ when task(negative, tv1) -> [(J(Inh(a, b)), (Some(neg(tv1)), Some(desireNeg(dv1))))]
           | _ -> []
       )

// Rule 37


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, b1)), Inh(b2, c) when a <> c && b1 = b2 -> [(J(Inh(a, c)), (Some(ded(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 38


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, b1)), Inh(b2, c) when a <> c && b1 = b2 -> [(Q(Inh(a, c)), (None, None))]
           | _ -> []
       )

// Rule 39


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a1, b)), Inh(a2, c) when b <> c && a1 = a2 -> [(J(Inh(b, c)), (Some(abd(tv1, tv2)), Some(desireWeak(dv1, dv2))))]
           | _ -> []
       )

// Rule 40


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a1, b)), Inh(a2, c) when b <> c && a1 = a2 -> [(Q(Inh(b, c)), (None, None))]
           | _ -> []
       )

// Rule 41


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, c1)), Inh(b, c2) when a <> c1 && c1 = c2 -> [(J(Inh(a, b)), (Some(ind(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 42


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, c1)), Inh(b, c2) when a <> c1 && c1 = c2 -> [(Q(Inh(a, b)), (None, None))]
           | _ -> []
       )

// Rule 43


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, b1)), Inh(b2, c) when a <> c && b1 = b2 -> [(J(Inh(c, a)), (Some(exe(tv1, tv2)), Some(desireWeak(dv1, dv2))))]
           | _ -> []
       )

// Rule 44


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, b1)), Inh(b2, c) when a <> c && b1 = b2 -> [(Q(Inh(c, a)), (None, None))]
           | _ -> []
       )

// Rule 45


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, p1)), Inh(s2, p2) when s1 = s2 && p1 = p2 -> [(J(Sim(s1, p1)), (Some(int(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 46


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(s1, p1)), Inh(s2, p2) when s1 = s2 && p1 = p2 -> [(Q(Sim(s1, p1)), (None, None))]
           | _ -> []
       )

// Rule 47


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(s1, p1)), Inh(p2, s2) when s1 = s2 && p1 = p2 -> [(J(Inh(s1, p1)), (Some(redCon(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 48


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(s1, p1)), Inh(p2, s2) when s1 = s2 && p1 = p2 -> [(Q(Inh(s1, p1)), (None, None))]
           | _ -> []
       )

// Rule 49


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 -> [(J(Sim(s, p)), (Some(com(tv1, tv2)), Some(desireWeak(dv1, dv2))))]
           | _ -> []
       )

// Rule 50


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 51


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Inh(m2, s) when m1 = m2 && s <> p -> [(J(Sim(s, p)), (Some(com(tv1, tv2)), Some(desireWeak(dv1, dv2))))]
           | _ -> []
       )

// Rule 52


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(m1, p)), Inh(m2, s) when m1 = m2 && s <> p -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 53


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Sim(s, m2) when s <> p && m1 = m2 -> [(J(Inh(s, p)), (Some(ana(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 54


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(m1, p)), Sim(s, m2) when s <> p && m1 = m2 -> [(Q(Inh(s, p)), (None, None))]
           | _ -> []
       )

// Rule 55


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Sim(s, m2) when s <> p && m1 = m2 -> [(J(Inh(p, s)), (Some(ana(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 56


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(p, m1)), Sim(s, m2) when s <> p && m1 = m2 -> [(Q(Inh(p, s)), (None, None))]
           | _ -> []
       )

// Rule 57


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(m1, p)), Inh(s, m2) when s <> p && m1 = m2 -> [(J(Sim(s, p)), (Some(res(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 58


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(m1, p)), Inh(s, m2) when s <> p && m1 = m2 -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 59


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(IntInt(sort [s; p]), m1)), (Some(int(tv1, tv2)), None)    )]
           | _ -> []
       )

// Rule 60


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(ExtInt(sort [s; p]), m1)), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 61


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(IntDif(s, p), m1)), (Some(dif(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 62


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Inh(m2, s) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(m1, ExtInt(sort [p; s]))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 63


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Inh(m2, s) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(m1, IntInt(sort [p; s]))), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 64


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Inh(m2, s) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(m1, ExtDif(p, s))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 65


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(IntInt(a_1_n), m2) when m1 = m2 && a_1_n |> List.contains s -> [(J(Inh(reduce(IntInt(listRemove s a_1_n)), m1)), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 66


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(ExtInt(a_1_n), m2) when m1 = m2 && a_1_n |> List.contains s -> [(J(Inh(reduce(ExtInt(listRemove s a_1_n)), m1)), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 67


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, m1)), Inh(IntDif(s2, p), m2) when s1 = s2 && m1 = m2 -> [(J(Inh(p, m1)), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 68


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, m1)), Inh(IntDif(p, s2), m2) when s1 = s2 && m1 = m2 -> [(J(Inh(p, m1)), (Some(nnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 69


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, ExtInt(a_1_n)) when m1 = m2 && a_1_n |> List.contains s -> [(J(Inh(m1, reduce(ExtInt(listRemove s a_1_n)))), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 70


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, IntInt(a_1_n)) when m1 = m2 && a_1_n |> List.contains s -> [(J(Inh(m1, reduce(IntInt(listRemove s a_1_n)))), (Some(npp(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 71


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s1)), Inh(m2, ExtDif(s2, p)) when m1 = m2 && s1 = s2 -> [(J(Inh(m1, p)), (Some(pnp(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 72


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s1)), Inh(m2, ExtDif(p, s2)) when m1 = m2 && s1 = s2 -> [(J(Inh(m1, p)), (Some(nnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 73


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, ExtSet(a))), Inh(c2, ExtSet(b)) when c1 = c2 -> [(J(Inh(c1, reduce(ExtSet(union a b)))), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 74


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, IntSet(a))), Inh(c2, IntSet(b)) when c1 = c2 -> [(J(Inh(c1, reduce(IntSet(union a b)))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 75


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(ExtSet(a), c1)), Inh(ExtSet(b), c2) when c1 = c2 -> [(J(Inh(reduce(ExtSet(union a b)), c2)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 76


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(IntSet(a), c1)), Inh(IntSet(b), c2) when c1 = c2 -> [(J(Inh(reduce(IntSet(union a b)), c2)), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 77


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, ExtSet(a))), Inh(c2, ExtSet(b)) when c1 = c2 && isIntersection a b -> [(J(Inh(c1, reduce(ExtSet(intersect a b)))), (Some(int(tv1, tv2)), None)    )]
           | _ -> []
       )

// Rule 78


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, IntSet(a))), Inh(c2, IntSet(b)) when c1 = c2 && isIntersection a b -> [(J(Inh(c1, reduce(IntSet(intersect a b)))), (Some(uni(tv1, tv2)), None)   )]
           | _ -> []
       )

// Rule 79


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(ExtSet(a), c1)), Inh(ExtSet(b), c2) when c1 = c2 && isIntersection a b -> [(J(Inh(reduce(ExtSet(intersect a b)), c1)), (Some(uni(tv1, tv2)), None)   )]
           | _ -> []
       )

// Rule 80


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(IntSet(a), c1)), Inh(IntSet(b), c2) when c1 = c2 && isIntersection a b -> [(J(Inh(reduce(IntSet(intersect a b)), c1)), (Some(int(tv1, tv2)), None)   )]
           | _ -> []
       )

// Rule 81


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, ExtSet(a))), Inh(c2, ExtSet(b)) when c1 = c2 && isDiff a b -> [(J(Inh(c1, reduce(ExtSet(diff a b)))), (Some(dif(tv1, tv2)), None)        )]
           | _ -> []
       )

// Rule 82


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, ExtSet(a))), Inh(c2, ExtSet(b)) when c1 = c2 && isDiff a b -> [(J(Inh(c1, reduce(IntSet(diff a b)))), (Some(dif(tv1, tv2)), None)        )]
           | _ -> []
       )

// Rule 83


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(c, ExtSet(a_i::a_1_n)) -> [(J(Inh(c, ExtSet([a_i]))), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 84


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(c, IntSet(a_i::a_1_n)) -> [(J(Inh(c, IntSet([a_i]))), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 85


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(ExtSet(a_i::a_1_n), c) -> [(J(Inh(ExtSet([a_i]), c)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 86


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntSet(a_i::a_1_n), c) -> [(J(Inh(IntSet([a_i]), c)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 87


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntInt(a_i::a_1_n), m) -> [(J(Inh(a_i, m)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 88


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(m, ExtInt(a_i::a_1_n)) -> [(J(Inh(m, a_i)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 89


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntDif(b, g), s) -> [(J(Inh(b, s)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 90


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(r, ExtDif(b, s))), _ -> [(J(Inh(r, b)), (Some(structuralDed(tv1)), None))]
           | _ -> []
       )

// Rule 91


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(Prod([a_i; x]), m) when m <> a_i -> [(J(Inh(a_i, ExtImg(m::(sub_ a_i [a_i; x])))), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 92


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(Prod([x; a_i]), m) when m <> a_i -> [(J(Inh(a_i, ExtImg(m::(sub_ a_i [x; a_i])))), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 93


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(m, Prod([a_i; x])) when m <> a_i -> [(J(Inh(IntImg(m::(sub_ a_i [a_i; x])), a_i)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 94


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(m, Prod([x; a_i])) when m <> a_i -> [(J(Inh(IntImg(m::(sub_ a_i [x; a_i])), a_i)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 95


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(a_i, ExtImg(m::(Constant "_")::a_1_n)) -> [(J(Inh(subst(Prod((Constant "_")::a_1_n)) (Constant "_") a_i, m)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 96


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(a_i, ExtImg(m::x::(Constant "_")::a_1_n)) -> [(J(Inh(subst(Prod(x::(Constant "_")::a_1_n)) (Constant "_") a_i, m)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 97


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntImg(m::(Constant "_")::a_1_n), a_i) -> [(J(Inh(m, subst(Prod((Constant "_")::a_1_n)) (Constant "_") a_i)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 98


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntImg(m::x::(Constant "_")::a_1_n), a_i) -> [(J(Inh(m, subst(Prod(x::(Constant "_")::a_1_n)) (Constant "_") a_i)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 99


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, s1)), Inh(b, s2) when s1 = s2 -> [(Q(Inh(a, b)), (None, None))]
           | _ -> []
       )

// Rule 100


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, s1)), Inh(b, s2) when s1 = s2 -> [(Q(Inh(b, a)), (None, None))]
           | _ -> []
       )

// Rule 101


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(IntSet([a1]), IntSet([b1]))), Sim(a2, b2) when a1 = a2 && b1 = b2 && a1 <> b1 -> [(J(Sim(IntSet([a1]), IntSet([b1]))), (Some(beliefId(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 102


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(ExtSet([a1]), ExtSet([b1]))), Sim(a2, b2) when a1 = a2 && b1 = b2 && a1 <> b1 -> [(J(Sim(ExtSet([a1]), ExtSet([b1]))), (Some(beliefId(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 103


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntSet([a1]), IntSet([b1]))), Sim(a2, b2) when a1 = a1 && b1 = b2 && a1 <> b1 -> [(J(Inh(IntSet([a1]), IntSet([b1]))), (Some(beliefId(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 104


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtSet([a1]), ExtSet([b1]))), Sim(a2, b2) when a1 = a2 && b1 = b2 && a1 <> b1 -> [(J(Inh(ExtSet([a1]), ExtSet([b1]))), (Some(beliefId(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 105


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtInt(a_1_n), ExtInt(b_1_n))), Inh(b, a) when (a_1_n |> List.contains b) && (b_1_n |> List.contains a) && ((set a_1_n) |> Set.remove b) = ((set b_1_n) |> Set.remove a) -> [(J(Inh(ExtInt(sort(a_1_n)), ExtInt(sort(b_1_n)))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 106


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntInt(a_1_n), IntInt(b_1_n))), Inh(b, a) when (a_1_n |> List.contains b) && (b_1_n |> List.contains a) && ((set a_1_n) |> Set.remove b) = ((set b_1_n) |> Set.remove a) -> [(J(Inh(IntInt(sort(a_1_n)), IntInt(sort(b_1_n)))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 107


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtDif(s1, a1), ExtDif(s2, b1))), Inh(b2, a2) when a1 = a2 && b1 = b1 && s1 = s2 && a1 <> b1 -> [(J(Inh(ExtDif(s1, a1), ExtDif(s1, b1))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 108


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntDif(s1, a1), IntDif(s2, b1))), Inh(b2, a2) when a1 = a2 && b1 = b1 && s1 = s2 && a1 <> b1 -> [(J(Inh(IntDif(s1, b1), IntDif(s1, a1))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 109


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(w1, IntInt(a_1_n))), Inh(w2, b) when w1 = w2 && a_1_n |> List.contains b -> [(J(Inh(w1, reduce(IntInt(sort(a_1_n))))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 110


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtInt(a_1_n), w1)), Inh(b, w2) when w1 = w2 && a_1_n |> List.contains b -> [(J(Inh(reduce(ExtInt(sort(a_1_n))), w1)), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 111


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(w1, ExtDif(s, b1))), Inh(w2, b2) when b1 = b2 && w1 = w2 -> [(J(Inh(w1, reduce(ExtDif(s, b)))), (Some(beliefStructuralDif(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 112


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntDif(s, b1), w1)), Inh(b2, w2) when b1 = b2 && w1 = w2 -> [(J(Inh(reduce(IntDif(s, b1)), w1)), (Some(beliefStructuralDif(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 113


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(Prod([b1; p]), QVar(x))), Inh(b2, a) when b1 = b2 -> [(J(Inh(reduce(Prod([b1; p])), reduce(Prod([a; p])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 114


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(Prod([b1; p1]), Prod([a1; p2]))), Inh(b2, a2) when a1 = a2 && b1 = b2 -> [(J(Inh(reduce(Prod([b1; p1])), reduce(Prod([a1; p1])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 115


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntImg([n1; a; Constant "_"]), QVar(x))), Inh(n2, r) when n1 = n2 -> [(J(Inh(reduce(IntImg([n1; a; Constant "_"])), reduce(IntImg([r; a; Constant "_"])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 116


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntImg([n1; a1; Constant "_"]), IntImg([r1; a2; Constant "_"]))), Inh(n2, r2) when a1 = a2 && r1 = r2 && n1 = n2 -> [(J(Inh(reduce(IntImg([n1; a1; Constant "_"])), reduce(IntImg([r1; a1; Constant "_"])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 117


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtImg([n1; Constant "_"; b1]), QVar(X))), Inh(s, b2) when b1 = b2 -> [(J(Inh(reduce(ExtImg([n1; Constant "_"; b1])), reduce(ExtImg([n1; Constant "_"; s])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 118


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtImg([n1; Constant "_"; b1]), ExtImg([n2; Constant "_"; s1]))), Inh(s, b2) when b1 = b2 && n1 = n2 -> [(J(Inh(reduce(ExtImg([n1; Constant "_"; b])), reduce(IntImg([n1; Constant "_"; s])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )


    ]



