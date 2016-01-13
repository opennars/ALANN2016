
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
           | Q(Imp(p1, s1)), Imp(s2, p2) when s1 <> p1 && s1 = s2 && p1 = p2-> [(J(Imp(p1, s1)), (Some(cnv(tv2)), None))]
           | _ -> []
       )

// Rule 36


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(p1, s1)), ConImp(s2, p2) when s1 <> p1 && s1 = s2 && p1 = p2-> [(J(ConImp(p1, s1)), (Some(cnv(tv2)), None))]
           | _ -> []
       )

// Rule 37


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p1, s1)), RetImp(s2, p2) when s1 <> p1 && s1 = s2 && p1 = p2-> [(J(RetImp(p1, s1)), (Some(cnv(tv2)), None))]
           | _ -> []
       )

// Rule 38


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p1, s1)), PreImp(s2, p2) when s1 <> p1 && s1 = s2 && p1 = p2-> [(J(PreImp(p1, s1)), (Some(cnv(tv2)), None))]
           | _ -> []
       )

// Rule 39


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(Not(s1), p)), Not(s2) when s1 = s2 -> [(J(Imp(Not(p), s1)), (Some(cnt(tv1)), None))]
           | _ -> []
       )

// Rule 40


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(Not(s1), p)), Not(s2) when s1 = s2 -> [(J(ConImp(Not(p), s1)), (Some(cnt(tv1)), None))]
           | _ -> []
       )

// Rule 41


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(Not(s1), p)), Not(s2) when s1 = s2 -> [(J(PreImp(Not(p), s1)), (Some(cnt(tv1)), None))]
           | _ -> []
       )

// Rule 42


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(Not(s1), p)), Not(s2) when s1 = s2 -> [(J(RetImp(Not(p), s1)), (Some(cnt(tv1)), None))]
           | _ -> []
       )

// Rule 43


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, b)), _ when task(negative, tv1) -> [(J(Not(Inh(a, b))), (Some(neg(tv1)), Some(desireNeg(dv1))))]
           | _ -> []
       )

// Rule 44


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Not(Inh(a, b))), _ when task(negative, tv1) -> [(J(Inh(a, b)), (Some(neg(tv1)), Some(desireNeg(dv1))))]
           | _ -> []
       )

// Rule 45


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, b1)), Inh(b2, c) when a <> c && b1 = b2 -> [(J(Inh(a, c)), (Some(ded(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 46


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, b1)), Inh(b2, c) when a <> c && b1 = b2 -> [(Q(Inh(a, c)), (None, None))]
           | _ -> []
       )

// Rule 47


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a1, b)), Inh(a2, c) when b <> c && a1 = a2 -> [(J(Inh(b, c)), (Some(abd(tv1, tv2)), Some(desireWeak(dv1, dv2))))]
           | _ -> []
       )

// Rule 48


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a1, b)), Inh(a2, c) when b <> c && a1 = a2 -> [(Q(Inh(b, c)), (None, None))]
           | _ -> []
       )

// Rule 49


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, c1)), Inh(b, c2) when a <> c1 && c1 = c2 -> [(J(Inh(a, b)), (Some(ind(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 50


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, c1)), Inh(b, c2) when a <> c1 && c1 = c2 -> [(Q(Inh(a, b)), (None, None))]
           | _ -> []
       )

// Rule 51


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, b1)), Inh(b2, c) when a <> c && b1 = b2 -> [(J(Inh(c, a)), (Some(exe(tv1, tv2)), Some(desireWeak(dv1, dv2))))]
           | _ -> []
       )

// Rule 52


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, b1)), Inh(b2, c) when a <> c && b1 = b2 -> [(Q(Inh(c, a)), (None, None))]
           | _ -> []
       )

// Rule 53


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, p1)), Inh(s2, p2) when s1 = s2 && p1 = p2 -> [(J(Sim(s1, p1)), (Some(int(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 54


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(s1, p1)), Inh(s2, p2) when s1 = s2 && p1 = p2 -> [(Q(Sim(s1, p1)), (None, None))]
           | _ -> []
       )

// Rule 55


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(s1, p1)), Inh(p2, s2) when s1 = s2 && p1 = p2 -> [(J(Inh(s1, p1)), (Some(redCon(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 56


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(s1, p1)), Inh(p2, s2) when s1 = s2 && p1 = p2 -> [(Q(Inh(s1, p1)), (None, None))]
           | _ -> []
       )

// Rule 57


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 -> [(J(Sim(s, p)), (Some(com(tv1, tv2)), Some(desireWeak(dv1, dv2))))]
           | _ -> []
       )

// Rule 58


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 59


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Inh(m2, s) when m1 = m2 && s <> p -> [(J(Sim(s, p)), (Some(com(tv1, tv2)), Some(desireWeak(dv1, dv2))))]
           | _ -> []
       )

// Rule 60


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(m1, p)), Inh(m2, s) when m1 = m2 && s <> p -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 61


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Sim(s, m2) when s <> p && m1 = m2 -> [(J(Inh(s, p)), (Some(ana(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 62


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(m1, p)), Sim(s, m2) when s <> p && m1 = m2 -> [(Q(Inh(s, p)), (None, None))]
           | _ -> []
       )

// Rule 63


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Sim(s, m2) when s <> p && m1 = m2 -> [(J(Inh(p, s)), (Some(ana(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 64


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(p, m1)), Sim(s, m2) when s <> p && m1 = m2 -> [(Q(Inh(p, s)), (None, None))]
           | _ -> []
       )

// Rule 65


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Sim(m1, p)), Inh(s, m2) when s <> p && m1 = m2 -> [(J(Sim(s, p)), (Some(res(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 66


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(m1, p)), Inh(s, m2) when s <> p && m1 = m2 -> [(Q(Sim(s, p)), (None, None))]
           | _ -> []
       )

// Rule 67


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(IntInt(sort [s; p]), m1)), (Some(int(tv1, tv2)), None)    )]
           | _ -> []
       )

// Rule 68


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(ExtInt(sort [s; p]), m1)), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 69


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(p, m1)), Inh(s, m2) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(IntDif(s, p), m1)), (Some(dif(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 70


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Inh(m2, s) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(m1, ExtInt(sort [p; s]))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 71


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Inh(m2, s) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(m1, IntInt(sort [p; s]))), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 72


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, p)), Inh(m2, s) when s <> p && m1 = m2 && not_set(s) && not_set(p) && no_common_subterm(s, p) -> [(J(Inh(m1, ExtDif(p, s))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 73


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(IntInt(a_1_n), m2) when m1 = m2 && a_1_n |> List.contains s -> [(J(Inh(reduce(IntInt(listRemove s a_1_n)), m1)), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 74


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(ExtInt(a_1_n), m2) when m1 = m2 && a_1_n |> List.contains s -> [(J(Inh(reduce(ExtInt(listRemove s a_1_n)), m1)), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 75


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, m1)), Inh(IntDif(s2, p), m2) when s1 = s2 && m1 = m2 -> [(J(Inh(p, m1)), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 76


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, m1)), Inh(IntDif(p, s2), m2) when s1 = s2 && m1 = m2 -> [(J(Inh(p, m1)), (Some(nnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 77


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, ExtInt(a_1_n)) when m1 = m2 && a_1_n |> List.contains s -> [(J(Inh(m1, reduce(ExtInt(listRemove s a_1_n)))), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 78


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, IntInt(a_1_n)) when m1 = m2 && a_1_n |> List.contains s -> [(J(Inh(m1, reduce(IntInt(listRemove s a_1_n)))), (Some(npp(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 79


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s1)), Inh(m2, ExtDif(s2, p)) when m1 = m2 && s1 = s2 -> [(J(Inh(m1, p)), (Some(pnp(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 80


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s1)), Inh(m2, ExtDif(p, s2)) when m1 = m2 && s1 = s2 -> [(J(Inh(m1, p)), (Some(nnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 81


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, ExtSet(a))), Inh(c2, ExtSet(b)) when c1 = c2 -> [(J(Inh(c1, reduce(ExtSet(union a b)))), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 82


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, IntSet(a))), Inh(c2, IntSet(b)) when c1 = c2 -> [(J(Inh(c1, reduce(IntSet(union a b)))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 83


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(ExtSet(a), c1)), Inh(ExtSet(b), c2) when c1 = c2 -> [(J(Inh(reduce(ExtSet(union a b)), c2)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 84


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(IntSet(a), c1)), Inh(IntSet(b), c2) when c1 = c2 -> [(J(Inh(reduce(IntSet(union a b)), c2)), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 85


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, ExtSet(a))), Inh(c2, ExtSet(b)) when c1 = c2 && isIntersection a b -> [(J(Inh(c1, reduce(ExtSet(intersect a b)))), (Some(int(tv1, tv2)), None)    )]
           | _ -> []
       )

// Rule 86


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, IntSet(a))), Inh(c2, IntSet(b)) when c1 = c2 && isIntersection a b -> [(J(Inh(c1, reduce(IntSet(intersect a b)))), (Some(uni(tv1, tv2)), None)   )]
           | _ -> []
       )

// Rule 87


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(ExtSet(a), c1)), Inh(ExtSet(b), c2) when c1 = c2 && isIntersection a b -> [(J(Inh(reduce(ExtSet(intersect a b)), c1)), (Some(uni(tv1, tv2)), None)   )]
           | _ -> []
       )

// Rule 88


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(IntSet(a), c1)), Inh(IntSet(b), c2) when c1 = c2 && isIntersection a b -> [(J(Inh(reduce(IntSet(intersect a b)), c1)), (Some(int(tv1, tv2)), None)   )]
           | _ -> []
       )

// Rule 89


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, ExtSet(a))), Inh(c2, ExtSet(b)) when c1 = c2 && isDiff a b -> [(J(Inh(c1, reduce(ExtSet(diff a b)))), (Some(dif(tv1, tv2)), None)        )]
           | _ -> []
       )

// Rule 90


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(c1, ExtSet(a))), Inh(c2, ExtSet(b)) when c1 = c2 && isDiff a b -> [(J(Inh(c1, reduce(IntSet(diff a b)))), (Some(dif(tv1, tv2)), None)        )]
           | _ -> []
       )

// Rule 91


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(c, ExtSet(a_i::a_1_n)) -> [(J(Inh(c, ExtSet([a_i]))), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 92


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(c, IntSet(a_i::a_1_n)) -> [(J(Inh(c, IntSet([a_i]))), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 93


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(ExtSet(a_i::a_1_n), c) -> [(J(Inh(ExtSet([a_i]), c)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 94


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntSet(a_i::a_1_n), c) -> [(J(Inh(IntSet([a_i]), c)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 95


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntInt(a_i::a_1_n), m) -> [(J(Inh(a_i, m)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 96


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(m, ExtInt(a_i::a_1_n)) -> [(J(Inh(m, a_i)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 97


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntDif(b, g), s) -> [(J(Inh(b, s)), (Some(structuralDed(tv2)), None))]
           | _ -> []
       )

// Rule 98


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(r, ExtDif(b, s))), _ -> [(J(Inh(r, b)), (Some(structuralDed(tv1)), None))]
           | _ -> []
       )

// Rule 99


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(Prod([a_i; x]), m) when m <> a_i -> [(J(Inh(a_i, ExtImg(m::(sub_ a_i [a_i; x])))), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 100


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(Prod([x; a_i]), m) when m <> a_i -> [(J(Inh(a_i, ExtImg(m::(sub_ a_i [x; a_i])))), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 101


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(m, Prod([a_i; x])) when m <> a_i -> [(J(Inh(IntImg(m::(sub_ a_i [a_i; x])), a_i)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 102


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(m, Prod([x; a_i])) when m <> a_i -> [(J(Inh(IntImg(m::(sub_ a_i [x; a_i])), a_i)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 103


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(a_i, ExtImg(m::(Constant "_")::a_1_n)) -> [(J(Inh(subst(Prod((Constant "_")::a_1_n)) (Constant "_") a_i, m)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 104


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(a_i, ExtImg(m::x::(Constant "_")::a_1_n)) -> [(J(Inh(subst(Prod(x::(Constant "_")::a_1_n)) (Constant "_") a_i, m)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 105


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntImg(m::(Constant "_")::a_1_n), a_i) -> [(J(Inh(m, subst(Prod((Constant "_")::a_1_n)) (Constant "_") a_i)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 106


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | _, Inh(IntImg(m::x::(Constant "_")::a_1_n), a_i) -> [(J(Inh(m, subst(Prod(x::(Constant "_")::a_1_n)) (Constant "_") a_i)), (Some(identity(tv2)), Some(identity(dv2))))]
           | _ -> []
       )

// Rule 107


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(m1, p)), Imp(s, m2) when s <> p && m1 = m2 -> [(J(Imp(s, p)), (Some(ded(tv1, tv2)), None) )]
           | _ -> []
       )

// Rule 108


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(m1, p)), Imp(s, m2) when s <> p && m1 = m2 -> [(Q(Imp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 109


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(p, m1)), Imp(s, m2) when m1 = m2 && s <> p -> [(J(Imp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 110


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(p, m1)), Imp(s, m2) when m1 = m2 && s <> p -> [(Q(Imp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 111


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(p, m1)), ConImp(s, m2) when p <> s && m1 = m2 -> [(J(ConImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 112


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(p, m1)), ConImp(s, m2) when p <> s && m1 = m2 -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 113


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(p, m1)), ConImp(s, m2) when p <> s && m1 = m2 -> [(J(PreImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 114


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(p, m1)), ConImp(s, m2) when p <> s && m1 = m2 -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 115


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(p, m1)), ConImp(s, m2) when p <> s && m1 = m2 -> [(J(RetImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 116


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(p, m1)), ConImp(s, m2) when p <> s && m1 = m2 -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 117


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(J(ConImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 118


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 119


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(J(PreImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 120


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 121


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(J(RetImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 122


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 123


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(J(ConImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 124


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 125


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(J(PreImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 126


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 127


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(J(RetImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 128


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 129


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(m1, p)), Imp(m2, s) when p <> s && m1 = m2 -> [(J(Imp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 130


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(m1, p)), Imp(m2, s) when p <> s && m1 = m2 -> [(Q(Imp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 131


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(J(ConImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 132


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 133


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(J(PreImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 134


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 135


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(J(RetImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 136


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 137


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(m1,p)), ConImp(m2, s) when p <> s && m1 = m2 -> [(J(ConImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 138


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(m1,p)), ConImp(m2, s) when p <> s && m1 = m2 -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 139


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(m1,p)), ConImp(m2, s) when p <> s && m1 = m2 -> [(J(PreImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 140


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(m1,p)), ConImp(m2, s) when p <> s && m1 = m2 -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 141


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(m1,p)), ConImp(m2, s) when p <> s && m1 = m2 -> [(J(RetImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 142


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(m1,p)), ConImp(m2, s) when p <> s && m1 = m2 -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 143


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(m1,p)), RetImp(m2, s) when p <> s && m1 = m2 -> [(J(ConImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 144


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(m1,p)), RetImp(m2, s) when p <> s && m1 = m2 -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 145


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(m1,p)), RetImp(m2, s) when p <> s && m1 = m2 -> [(J(PreImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 146


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(m1,p)), RetImp(m2, s) when p <> s && m1 = m2 -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 147


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(m1,p)), RetImp(m2, s) when p <> s && m1 = m2 -> [(J(RetImp(s, p)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 148


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(m1,p)), RetImp(m2, s) when p <> s && m1 = m2 -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 149


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(p, m1)), Imp(m2, s) when p <> s && m1 = m2 -> [(J(Imp(s, p)), (Some(exe(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 150


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(p, m1)), Imp(m2, s) when p <> s && m1 = m2 -> [(Q(Imp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 151


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(m2, s) when p <> s && m1 = m2 -> [(J(RetImp(s, p)), (Some(exe(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 152


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p, m1)), PreImp(m2, s) when p <> s && m1 = m2 -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 153


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(m2, s) when p <> s && m1 = m2 -> [(J(PreImp(s, p)), (Some(exe(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 154


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p, m1)), RetImp(m2, s) when p <> s && m1 = m2 -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 155


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(p, m1)), ConImp(m2, s) when p <> s && m1 = m2 -> [(J(ConImp(s, p)), (Some(exe(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 156


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(p, m1)), ConImp(m2, s) when p <> s && m1 = m2 -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 157


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(s1, p1)), Imp(p2, s2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(J(Equ(s1, p1)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 158


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(s1, p1)), Imp(p2, s2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(Q(Equ(s1, p1)), (None, None))]
           | _ -> []
       )

// Rule 159


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(s1, p1)), ConImp(p2, s2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(J(ConEqu(s1, p1)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 160


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(s1, p1)), ConImp(p2, s2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(Q(ConEqu(s1, p1)), (None, None))]
           | _ -> []
       )

// Rule 161


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(s1, p1)), RetImp(p2, s2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(J(PreEqu(s1, p1)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 162


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(s1, p1)), RetImp(p2, s2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(Q(PreEqu(s1, p1)), (None, None))]
           | _ -> []
       )

// Rule 163


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(s1, p1)), PreImp(p2, s2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(J(PreEqu(p1, s1)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 164


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(s1, p1)), PreImp(p2, s2) when s1 = s2 && p1 = p2 && s1 <> p1 -> [(Q(PreEqu(p1, s1)), (None, None))]
           | _ -> []
       )

// Rule 165


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(p, m1)), Imp(s, m2) when m1 = m2 && s <> p -> [(J(Equ(s, p)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 166


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(p, m1)), Imp(s, m2) when m1 = m2 && s <> p -> [(Q(Equ(s, p)), (None, None))]
           | _ -> []
       )

// Rule 167


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(J(ConEqu(s, p)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 168


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(Q(ConEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 169


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(J(PreEqu(s, p)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 170


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(Q(PreEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 171


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(J(PreEqu(p, s)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 172


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p, m1)), PreImp(s, m2) when p <> s && m1 = m2 -> [(Q(PreEqu(p, s)), (None, None))]
           | _ -> []
       )

// Rule 173


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(p, m1)), ConImp(s, m2) when m1 = m2 && s <> p -> [(J(ConEqu(s, p)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 174


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(p, m1)), ConImp(s, m2) when m1 = m2 && s <> p -> [(Q(ConEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 175


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(J(ConImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 176


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 177


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(J(PreEqu(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 178


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(Q(PreEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 179


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(J(RetImp(s, p)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 180


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p, m1)), RetImp(s, m2) when p <> s && m1 = m2 -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 181


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(m1, p)), Imp(m2, s) when p <> s && m1 = m2 -> [(J(Equ(s, p)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 182


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(m1, p)), Imp(m2, s) when p <> s && m1 = m2 -> [(Q(Equ(s, p)), (None, None))]
           | _ -> []
       )

// Rule 183


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(J(ConEqu(s, p)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 184


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(Q(ConEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 185


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(J(PreEqu(s, p)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 186


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(Q(PreEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 187


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(J(PreEqu(p, s)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 188


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(m1,p)), PreImp(m2, s) when p <> s && m1 = m2 -> [(Q(PreEqu(p, s)), (None, None))]
           | _ -> []
       )

// Rule 189


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(m1,p)), ConImp(m2, s) when p <> s && m1 = m2 -> [(J(ConEqu(s, p)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 190


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(m1,p)), ConImp(m2, s) when p <> s && m1 = m2 -> [(Q(ConEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 191


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(m1, p)), Equ(s, m2) when m1 = m2 && s <> p -> [(J(Imp(s, p)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 192


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(m1, p)), Equ(s, m2) when m1 = m2 && s <> p -> [(Q(Imp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 193


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1, p)), PreEqu(s, m2) when m1 = m2 && s <> p -> [(J(PreImp(s, p)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 194


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(m1, p)), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 195


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1, p)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(J(PreImp(s, p)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 196


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(m1, p)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Q(PreImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 197


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(m1, p)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(J(ConImp(s, p)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 198


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(m1, p)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Q(ConImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 199


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(m1, p)), PreEqu(m2, s) when m1 = m2 && s <> p -> [(J(RetImp(s, p)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 200


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(m1, p)), PreEqu(m2, s) when m1 = m2 && s <> p -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 201


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(m1, p)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(J(RetImp(s, p)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 202


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(m1, p)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Q(RetImp(s, p)), (None, None))]
           | _ -> []
       )

// Rule 203


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(p, m1)), Equ(s, m2) when m1 = m2 && s <> p -> [(J(Imp(p, s)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 204


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Imp(p, m1)), Equ(s, m2) when m1 = m2 && s <> p -> [(Q(Imp(p, s)), (None, None))]
           | _ -> []
       )

// Rule 205


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(J(PreImp(p, s)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 206


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreImp(p, m1)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Q(PreImp(p, s)), (None, None))]
           | _ -> []
       )

// Rule 207


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(p, m1)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(J(ConImp(p, s)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 208


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConImp(p, m1)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Q(ConImp(p, s)), (None, None))]
           | _ -> []
       )

// Rule 209


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Q(RetImp(p, s)), (None, None))]
           | _ -> []
       )

// Rule 210


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), PreEqu(s, m2) when m1 = m2 && s <> p -> [(J(RetImp(p, s)), (Some(ana(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 211


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(RetImp(p, m1)), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Q(RetImp(p, s)), (None, None))]
           | _ -> []
       )

// Rule 212


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Equ(m1, p)), Equ(s, m2) when m1 = m2 && s <> p -> [(J(Equ(s, p)), (Some(res(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 213


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Equ(m1, p)), Equ(s, m2) when m1 = m2 && s <> p -> [(Q(Equ(s, p)), (None, None))]
           | _ -> []
       )

// Rule 214


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreEqu(m1, p)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(J(PreEqu(s, p)), (Some(res(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 215


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(PreEqu(m1, p)), ConEqu(s, m2) when m1 = m2 && s <> p -> [(Q(PreEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 216


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConEqu(m1, p)), PreEqu(s, m2) when m1 = m2 && s <> p -> [(J(PreEqu(s, p)), (Some(res(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 217


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(ConEqu(m1, p)), PreEqu(s, m2) when m1 = m2 && s <> p -> [(Q(PreEqu(s, p)), (None, None))]
           | _ -> []
       )

// Rule 218


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(p, m1)), Imp(s, m2) when m1 = m2 && s <> p -> [(J(Imp(reduce(Or(sort [p; s])), m1)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 219


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(p, m1)), Imp(s, m2) when m1 = m2 && s <> p -> [(J(Imp(reduce(And(sort [p; s])), m1)), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 220


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(p, m1)), ConImp(s, m2) when m1 = m2 && s <> p -> [(J(ConImp(reduce(Or(sort [p; s])), m1)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 221


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(p, m1)), ConImp(s, m2) when m1 = m2 && s <> p -> [(J(ConImp(reduce(And(sort [p; s])), m1)), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 222


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(s, m2) when m1 = m2 && s <> p -> [(J(PreImp(reduce(Or(sort [p; s])), m1)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 223


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(p, m1)), PreImp(s, m2) when m1 = m2 && s <> p -> [(J(PreImp(reduce(And(sort [p; s])), m1)), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 224


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(s, m2) when m1 = m2 && s <> p -> [(J(RetImp(reduce(Or(sort [p; s])), m1)), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 225


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(p, m1)), RetImp(s, m2) when m1 = m2 && s <> p -> [(J(RetImp(reduce(And(sort [p; s])), m1)), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 226


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(m1, p)), Imp(m2, s) when m1 = m2 && s <> p -> [(J(Imp(m1, reduce(And(sort [p; s])))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 227


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(m1, p)), Imp(m2, s) when m1 = m2 && s <> p -> [(J(Imp(m1, reduce(Or(sort [p; s])))), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 228


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1, p)), PreImp(m2, s) when m1 = m2 && s <> p -> [(J(PreImp(m1, reduce(And(sort [p; s])))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 229


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(m1, p)), PreImp(m2, s) when m1 = m2 && s <> p -> [(J(PreImp(m1, reduce(Or(sort [p; s])))), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 230


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(m1, p)), ConImp(m2, s) when m1 = m2 && s <> p -> [(J(ConImp(m1, reduce(And(sort [p; s])))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 231


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(m1, p)), ConImp(m2, s) when m1 = m2 && s <> p -> [(J(ConImp(m1, reduce(Or(sort [p; s])))), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 232


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(m1, p)), RetImp(m2, s) when m1 = m2 && s <> p -> [(J(RetImp(m1, reduce(And(sort [p; s])))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 233


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(m1, p)), RetImp(m2, s) when m1 = m2 && s <> p -> [(J(RetImp(m1, reduce(Or(sort [p; s])))), (Some(uni(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 234


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(d1, r)), RetImp(d2, k) when d1 = d2 && r <> k -> [(J(PreImp(k, r)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 235


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(d1, r)), RetImp(d2, k) when d1 = d2 && r <> k -> [(J(RetImp(r, k)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 236


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(d1, r)), RetImp(d2, k) when d1 = d2 && r <> k -> [(J(PreEqu(k, r)), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 237


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(s, m1)), Imp(Or(a_1_n), m2) when m1 = m2 && a_1_n |> List.contains s -> [(J(Imp(reduce(Or(listRemove s a_1_n)), m1)), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 238


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(s, m1)), Imp(And(a_1_n), m2) when m1 = m2 && a_1_n |> List.contains s -> [(J(Imp(reduce(And(listRemove s a_1_n)), m1)), (Some(npp(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 239


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(m1, s)), Imp(m2, And(a_1_n)) when m1 = m2 && a_1_n |> List.contains s -> [(J(Imp(reduce(And(listRemove s a_1_n)), m1)), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 240


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(m1, s)), Imp(m2, Or(a_1_n)) when m1 = m2 && a_1_n |> List.contains s -> [(J(Imp(reduce(Or(listRemove s a_1_n)), m1)), (Some(npp(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 241


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(m1), Imp(m2, p) when m1 = m1 -> [(J(p), (Some(ded(tv1, tv2)), Some(desireInd(dv1, dv2))))]
           | _ -> []
       )

// Rule 242


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(m1), Imp(p, m2) when m1 = m2 -> [(J(p), (Some(abd(tv1, tv2)), Some(desireDed(dv1, dv2))) )]
           | _ -> []
       )

// Rule 243


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(m1), Equ(s, m2) when m1 = m2 -> [(J(s), (Some(ana(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 244


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(m1), Equ(m2, s) when m1 = m2 -> [(J(s), (Some(ana(tv1, tv2)), Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 245


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(And(a_1::a_1_n)), _ -> [(J(a_1), (Some(structuralDed(tv1)), Some(desireStructuralStrong(tv1))) )]
           | _ -> []
       )

// Rule 246


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | G(Seq(a_1_n)), b when a_1_n |> List.contains b -> [(G(Seq(a_1_n |> listRemove b)), (None, Some(desireStrong(dv1, dv2))))]
           | _ -> []
       )

// Rule 247


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(s), And(a_1_n) when a_1_n |> List.contains s -> [(J(reduce(And(listRemove s a_1_n))), (Some(pnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 248


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(s), Or(a_1_n) when a_1_n |> List.contains s -> [(J(reduce(Or(listRemove s a_1_n))), (Some(npp(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 249


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(s), And(a_1_n) when a_1_n |> List.contains (Not(s)) -> [(J(reduce(And(a_1_n |> listRemove (Not(s))))), (Some(nnn(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 250


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(And(a_1_n), c1)), Imp(And(b_1_n), c2) when ((set a_1_n) - (set b_1_n) |> Set.count) = 1 -> [(J(((set a_1_n) - (set b_1_n) |> Set.maxElement)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 251


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(And(a_1_n), c)), m when not_imp_or_equ(m) -> [(J(Imp(reduce(And(sort(m::a_1_n))), c)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 252


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(ConImp(And(a_1_n), c)), m when not_imp_or_equ(m) -> [(J(ConImp(reduce(And(sort(m::a_1_n))), c)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 253


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(PreImp(And(a_1_n), c)), m when not_imp_or_equ(m) -> [(J(PreImp(reduce(And(sort(m::a_1_n))), c)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 254


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(RetImp(And(a_1_n), c)), m when not_imp_or_equ(m) -> [(J(RetImp(reduce(And(sort(m::a_1_n))), c)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 255


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(And(a_1_n), c)), Imp(a, m) when a_1_n |> List.contains m -> [(J(Imp(reduce(And(sort(a::(a_1_n |> listRemove m)))), c)), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 256


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(And(m::a_1_n), c1)), Imp(And(a::b_1_n), c2) when c1 = c2 && a_1_n = b_1_n -> [(J(Imp(a, m)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 257


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(And(a_1_n), c)), Imp(a, m) when a_1_n |> List.contains a -> [(J(Imp(reduce(And(sort(m::(a_1_n |> listRemove a)))), c)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 258


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(Imp(Inh(p, IVar("X")), Inh(s, IVar("X")))), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 259


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(Imp(Inh(s, IVar("X")), Inh(s, IVar("X")))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 260


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(Equ(Inh(p, IVar("X")), Inh(s, IVar("X")))), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 261


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(reduce(And(sort([Inh(s, DVar("Y")); Inh(p, DVar("Y"))])))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 262


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(PreImp(Seq([Inh(p, IVar("X"))]), Inh(s, IVar("X")))), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 263


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(RetImp(Inh(s, IVar("X")), Seq([Inh(p, IVar("X"))]))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 264


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(PreEqu(Seq([Inh(p, IVar("X"))]), Inh(s, IVar("X")))), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 265


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(Seq([Inh(p, DVar("Y")); Inh(s, DVar("Y"))])), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 266


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(ConImp(Inh(p, IVar("X")), Inh(s, IVar("X")))), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 267


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(ConImp(Inh(s, IVar("X")), Inh(p, IVar("X")))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 268


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(ConEqu(Inh(p, IVar("X")), Inh(s, IVar("X")))), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 269


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, m1)), Inh(p, m2) when s <> p && m1 = m2 -> [(J(Par(sort [Inh(p, DVar("Y")); Inh(s, DVar("Y"))])), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 270


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(Imp(Inh(IVar("X"), s), Inh(IVar("X"), p))), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 271


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(Imp(Inh(IVar("X"), p), Inh(IVar("X"), s))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 272


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(Equ(Inh(IVar("X"), s), Inh(IVar("X"), p))), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 273


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(reduce(And(sort [Inh(DVar("Y"), s); Inh(DVar("Y"), p)]))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 274


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(PreImp(Seq([Inh(IVar("X"), s)]), Inh(IVar("X"), p))), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 275


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(RetImp(Inh(IVar("X"), p), Seq([Inh(IVar("X"), s)]))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 276


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(PreEqu(Seq([Inh(IVar("X"), s)]), Inh(IVar("X"), p))), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 277


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(Seq([Inh(DVar("Y"), s); Inh(DVar("Y"), p)])), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 278


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(ConImp(Inh(IVar("X"), s), Inh(IVar("X"), p))), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 279


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(ConImp(Inh(IVar("X"), p), Inh(IVar("X"), s))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 280


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(ConEqu(Inh(IVar("X"), s), Inh(IVar("X"), p))), (Some(com(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 281


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(m1, s)), Inh(m2, p) when s <> p && m1 = m2 -> [(J(Par(sort [Inh(DVar("Y"), s); Inh(DVar("Y"), p)])), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 282


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(a, Inh(m1, p))), Inh(m2, s) when m1 = m2 && a <> Inh(m1, s) && s <> p -> [(J(Imp(reduce(And(sort[a; Inh(IVar("Y"), s)])), Inh(IVar("Y"), s))), (Some(ind(tv1, tv2)), None) )]
           | _ -> []
       )

// Rule 283


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(a, Inh(m1, p))), Inh(m2, s) when m1 = m2 && a <> Inh(m1, s) && s <> p -> [(J(reduce(And(sort[Imp(a, Inh(DVar("Y"), p)); Inh(DVar("Y"), s)]))), (Some(int(tv1, tv2)), None) )]
           | _ -> []
       )

// Rule 284


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(And(Inh(m1, p)::a_1_n)), Inh(m2, s) when m1 = m2 && s <> p -> [(J(Imp(Inh(IVar("Y"), s), reduce(And(sort(Inh(IVar("Y"), p)::(a_1_n |> listRemove (Inh(m1, p)))))))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 285


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(And(Inh(m1, p)::a_1_n)), Inh(m2, s) when m1 = m2 && s <> p -> [(J(reduce(And(sort(Inh(DVar("Y"), s)::Inh(DVar("Y"), p)::(a_1_n |> listRemove (Inh(m1, p))))))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 286


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(a, Inh(p, m1))), Inh(s, m2) when s <> p && m1 = m2 && s <> m1 -> [(J(Imp(reduce(And(sort [a; Inh(p, IVar("X"))])), Inh(s, IVar("X")))), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 287


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(a, Inh(p, m1))), Inh(s, m2) when s <> p && m1 = m2 && s <> m1 -> [(J(reduce(And(sort [Imp(a, Inh(p, DVar("Y"))); Inh(s, DVar("Y"))]))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 288


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(And(Inh(p, m1)::a_1_n)), Inh(s, m2) when s <> p && m1 = m2 -> [(J(Imp(Inh(s, IVar("Y")), reduce(And(sort (Inh(s, DVar("Y"))::Inh(p, DVar("Y"))::(a_1_n |> listRemove (Inh(p, m1)))))))), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 289


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(And(Inh(p, m1)::a_1_n)), Inh(s, m2) when s <> p && m1 = m2 -> [(J(reduce(And(sort(Inh(s, DVar("Y"))::Inh(p, DVar("Y"))::(a_1_n |> listRemove (Inh(p, m1))))))), (Some(int(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 290


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(Inh(a, r1), z1)), Imp(And([Inh(DVar(y1), b); Inh(DVar(y2), r2)]), z2) when z1 = z2 && r1 = r2 -> [(J(Inh(a, b)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 291


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(u, l1)), Imp(And([Inh(DVar(x), l2); Inh(DVar(y), r)]), z) when l1 = l2 && x = y -> [(J(Imp(Inh(u, r), z)), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 292


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(u, l1)), Imp(And(Inh(DVar(x), l2)::Inh(DVar(y), r)::a_1_n), z) when l1 = l2 && x = y -> [(J(Imp(reduce(And(sort(Inh(u, r)::a_1_n))), z)), (Some(ded(tv1, tv2)), None)        )]
           | _ -> []
       )

// Rule 293


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, p1)), Imp(Inh(IVar(x1), p2), Inh(IVar(x2), h)) when p1 = p2 && x1 = x2 -> [(J(Inh(s, h)), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 294


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, p)), Imp(Inh(s2, IVar(x1)), Inh(h, IVar(x2))) when s1 = s2 && x1 = x2 -> [(J(Inh(h, s1)), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 295


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, p1)), Imp(Inh(IVar(x1), h), Inh(IVar(x2), p2)) when p1 = p2 && x1 = x2 -> [(J(Inh(s, h)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 296


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, p)), Imp(Inh(h, IVar(x1)), Inh(s2, IVar(x2))) when s1 = s2 && x1 = x2 -> [(J(Inh(h, s1)), (Some(abd(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 297


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, p1)), Equ(Inh(IVar(x1), p2), Inh(IVar(x2), h)) when p1 = p2 && x1 = x2 -> [(J(Inh(s, h)), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 298


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, p)), Equ(Inh(s2, IVar(x1)), Inh(h, IVar(x2))) when s1 = s2 && x1 = x2 -> [(J(Inh(h, s1)), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 299


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s, p1)), Equ(Inh(IVar(x1), h), Inh(IVar(x2), p2)) when p1 = p2 && x1 = x2 -> [(J(Inh(s, h)), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 300


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(s1, p)), Equ(Inh(h, IVar(x1)), Inh(s2, IVar(x2))) when s1 = s2 && x1 = x2 -> [(J(Inh(h, s1)), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 301


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, k1)), Imp(And([Inh(DVar(x), l); Inh(IVar(y), k2)]), And(a_1_n)) when k1 = k2 && x <> y -> [(J(subst(reduce(And(sort(Inh(DVar(x), l)::a_1_n)))) (IVar(y)) a), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 302


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, k1)), Imp(And([Inh(IVar(y), k2); Inh(DVar(x), l)]), And(a_1_n)) when k1 = k2 && x <> y -> [(J(subst(reduce(And(sort(Inh(DVar(x), l)::a_1_n)))) (IVar(y)) a), (Some(ded(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 303


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a, k1)), Imp(Inh(IVar(x), l), And(Inh(DVar(y), k2)::a_1_n)) when k1 = k2 && a_1_n |> List.contains (Inh(DVar(y), k1)) -> [(J(subst(Imp(Inh(IVar(x), l), reduce(And(a_1_n |> listRemove (Inh(DVar(y), k1)))))) (DVar(y)) a), (Some(anonAna(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 304


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(And(a_1_n), z1)), Imp(And(b_1_n), z2) when z1 = z2 && commonSingleton b_1_n a_1_n -> [(J(Imp(reduce(And(removeCommon a_1_n b_1_n)), reduce(And(removeCommon b_1_n a_1_n)))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 305


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(And(a_1_n), z1)), Imp(And(b_1_n), z2) when z1 = z2 && commonSingleton b_1_n a_1_n -> [(J(Imp(reduce(And(removeCommon b_1_n a_1_n)), reduce(And(removeCommon a_1_n b_1_n)))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 306


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(z1, And(a_1_n))), Imp(z2, And(b_1_n)) when z1 = z2 && commonSingleton b_1_n a_1_n -> [(J(Imp(reduce(And(removeCommon a_1_n b_1_n)), reduce(And(removeCommon b_1_n a_1_n)))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 307


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Imp(z1, And(a_1_n))), Imp(z2, And(b_1_n)) when z1 = z2 && commonSingleton b_1_n a_1_n -> [(J(Imp(reduce(And(removeCommon b_1_n a_1_n)), reduce(And(removeCommon a_1_n b_1_n)))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 308


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a1, l)), Imp(Inh(a2, s), r) when a1 = a2 -> [(J(Imp(reduce(And(sort([Inh(DVar("X"), l); Inh(DVar("X"), s)]))), r)), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 309


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | J(Inh(a1, l)), Imp(And(Inh(a2, s)::a_1_n), r) when a1 = a2 -> [(J(subst(Imp(reduce(And(sort(Inh(DVar("X"), l)::Inh(DVar("X"), s)::a_1_n))), r)) a1 (DVar("X"))), (Some(ind(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 310


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, s1)), Inh(b, s2) when s1 = s2 -> [(Q(Inh(a, b)), (None, None))]
           | _ -> []
       )

// Rule 311


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(a, s1)), Inh(b, s2) when s1 = s2 -> [(Q(Inh(b, a)), (None, None))]
           | _ -> []
       )

// Rule 312


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(IntSet([a1]), IntSet([b1]))), Sim(a2, b2) when a1 = a2 && b1 = b2 && a1 <> b1 -> [(J(Sim(IntSet([a1]), IntSet([b1]))), (Some(beliefId(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 313


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Sim(ExtSet([a1]), ExtSet([b1]))), Sim(a2, b2) when a1 = a2 && b1 = b2 && a1 <> b1 -> [(J(Sim(ExtSet([a1]), ExtSet([b1]))), (Some(beliefId(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 314


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntSet([a1]), IntSet([b1]))), Sim(a2, b2) when a1 = a1 && b1 = b2 && a1 <> b1 -> [(J(Inh(IntSet([a1]), IntSet([b1]))), (Some(beliefId(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 315


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtSet([a1]), ExtSet([b1]))), Sim(a2, b2) when a1 = a2 && b1 = b2 && a1 <> b1 -> [(J(Inh(ExtSet([a1]), ExtSet([b1]))), (Some(beliefId(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 316


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtInt(a_1_n), ExtInt(b_1_n))), Inh(b, a) when (a_1_n |> List.contains b) && (b_1_n |> List.contains a) && ((set a_1_n) |> Set.remove b) = ((set b_1_n) |> Set.remove a) -> [(J(Inh(ExtInt(sort(a_1_n)), ExtInt(sort(b_1_n)))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 317


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntInt(a_1_n), IntInt(b_1_n))), Inh(b, a) when (a_1_n |> List.contains b) && (b_1_n |> List.contains a) && ((set a_1_n) |> Set.remove b) = ((set b_1_n) |> Set.remove a) -> [(J(Inh(IntInt(sort(a_1_n)), IntInt(sort(b_1_n)))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 318


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtDif(s1, a1), ExtDif(s2, b1))), Inh(b2, a2) when a1 = a2 && b1 = b1 && s1 = s2 && a1 <> b1 -> [(J(Inh(ExtDif(s1, a1), ExtDif(s1, b1))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 319


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntDif(s1, a1), IntDif(s2, b1))), Inh(b2, a2) when a1 = a2 && b1 = b1 && s1 = s2 && a1 <> b1 -> [(J(Inh(IntDif(s1, b1), IntDif(s1, a1))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 320


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(w1, IntInt(a_1_n))), Inh(w2, b) when w1 = w2 && a_1_n |> List.contains b -> [(J(Inh(w1, reduce(IntInt(sort(a_1_n))))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 321


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtInt(a_1_n), w1)), Inh(b, w2) when w1 = w2 && a_1_n |> List.contains b -> [(J(Inh(reduce(ExtInt(sort(a_1_n))), w1)), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 322


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(w1, ExtDif(s, b1))), Inh(w2, b2) when b1 = b2 && w1 = w2 -> [(J(Inh(w1, reduce(ExtDif(s, b)))), (Some(beliefStructuralDif(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 323


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntDif(s, b1), w1)), Inh(b2, w2) when b1 = b2 && w1 = w2 -> [(J(Inh(reduce(IntDif(s, b1)), w1)), (Some(beliefStructuralDif(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 324


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(Prod([b1; p]), QVar(x))), Inh(b2, a) when b1 = b2 -> [(J(Inh(reduce(Prod([b1; p])), reduce(Prod([a; p])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 325


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(Prod([b1; p1]), Prod([a1; p2]))), Inh(b2, a2) when a1 = a2 && b1 = b2 -> [(J(Inh(reduce(Prod([b1; p1])), reduce(Prod([a1; p1])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 326


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntImg([n1; a; Constant "_"]), QVar(x))), Inh(n2, r) when n1 = n2 -> [(J(Inh(reduce(IntImg([n1; a; Constant "_"])), reduce(IntImg([r; a; Constant "_"])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 327


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(IntImg([n1; a1; Constant "_"]), IntImg([r1; a2; Constant "_"]))), Inh(n2, r2) when a1 = a2 && r1 = r2 && n1 = n2 -> [(J(Inh(reduce(IntImg([n1; a1; Constant "_"])), reduce(IntImg([r1; a1; Constant "_"])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 328


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtImg([n1; Constant "_"; b1]), QVar(X))), Inh(s, b2) when b1 = b2 -> [(J(Inh(reduce(ExtImg([n1; Constant "_"; b1])), reduce(ExtImg([n1; Constant "_"; s])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 329


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Inh(ExtImg([n1; Constant "_"; b1]), ExtImg([n2; Constant "_"; s1]))), Inh(s, b2) when b1 = b2 && n1 = n2 -> [(J(Inh(reduce(ExtImg([n1; Constant "_"; b])), reduce(IntImg([n1; Constant "_"; s])))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )

// Rule 330


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Not(a1)), a2 when a1 = a2 -> [(J(reduce(Not(a1))), (Some(neg(tv1)), None))]
           | _ -> []
       )

// Rule 331


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(a1), Not(a2) when a1 = a2 -> [(J(a1), (Some(neg(tv2)), None))]
           | _ -> []
       )

// Rule 332


    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           | Q(Or(a_1_n)), b when a_1_n |> List.contains b -> [(J(reduce(reduce(Or(sort(b::a_1_n))))), (Some(beliefStructuralDed(tv1, tv2)), None))]
           | _ -> []
       )


    ]

