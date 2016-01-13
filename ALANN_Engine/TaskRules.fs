module TaskRules

open Types
open TruthFunctions

let backwardTaskRules : RuleFunc list = 
    [

    // ********************
    // Backward Task Rules
    // ********************

//     (P --> S), (S --> P)task("?", t) |- (P --> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(p1, s1), Inh(s2, p2) when p1 = p2 && s1 = s2 -> [(Inh(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P --> S), (S --> P)task("?", t) |- (P --> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(p1, s1), Inh(s2, p2) when p1 = p2 && s1 = s2 -> [(Inh(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P ==> S), (S ==> P)task("?", t) |- (P ==> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Imp(p1, s1), Imp(s2, p2) when p1 = p2 && s1 = s2 -> [(Imp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P ==> S), (S ==> P)task("?", t) |- (P ==> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Imp(p1, s1), Imp(s2, p2) when p1 = p2 && s1 = s2 -> [(Imp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =|> S), (S =|> P)task("?", t) |- (P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | ConImp(p1, s1), ConImp(s2, p2) when p1 = p2 && s1 = s2 -> [(ConImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =|> S), (S =|> P)task("?", t) |- (P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | ConImp(p1, s1), ConImp(s2, p2) when p1 = p2 && s1 = s2 -> [(ConImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =|> S), (S =/> P)task("?", t) |- (P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | RetImp(p1, s1), PreImp(s2, p2) when p1 = p2 && s1 = s2 -> [(RetImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =|> S), (S =/> P)task("?", t) |- (P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | RetImp(p1, s1), PreImp(s2, p2) when p1 = p2 && s1 = s2 -> [(RetImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =/> S), (S =|> P)task("?", t) |- (P =/> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | PreImp(p1, s1), RetImp(s2, p2) when p1 = p2 && s1 = s2 -> [(PreImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =/> S), (S =|> P)task("?", t) |- (P =/> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | PreImp(p1, s1), RetImp(s2, p2) when p1 = p2 && s1 = s2 -> [(PreImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )

//     (A --> S), (B --> S)task("?", t) |- (A --> B), (NOT SHOWN)(B --> A), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(a, s1), Inh(b, s2) when s1 = s2 -> [(Inh(a, b), None, None)]
                | _ -> []
            )
//     (A --> S), (B --> S)task("?", t) |- (A --> B), (NOT SHOWN)(B --> A), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(a, s1), Inh(b, s2) when s1 = s2 -> [(Inh(b, a), None, None)]
                | _ -> []
            )
//     ([A] <-> [B]), (A <-> B)task("?", t) |- ([A] <-> [B]), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Sim(IntSet([a1]), IntSet([b1])), Sim(a2, b2) when a1 = a2 && b1 = b2 -> [(Sim(IntSet([a1]), IntSet([b1])), Some(beliefId(tv1, tv2)), None)]
                | _ -> []
            )
//     ({A} <-> {B}), (A <-> B)task("?", t) |- ({A} <-> {B}), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Sim(ExtSet([a1]), ExtSet([b1])), Sim(a2, b2) when a1 = a2 && b1 = b2 -> [(Sim(ExtSet([a1]), ExtSet([b1])), Some(beliefId(tv1, tv2)), None)]
                | _ -> []
            )
//     ([A] --> [B]), (A <-> B)task("?", t) |- ([A] --> [B]), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(IntSet([a1]), IntSet([b1])), Sim(a2, b2) when a1 = a2 && b1 = b2 -> [(Inh(IntSet([a1]), IntSet([b1])), Some(beliefId(tv1, tv2)), None)]
                | _ -> []
            )
//     ({A} --> {B}), (A <-> B)task("?", t) |- ({A} --> {B}), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(ExtSet([a1]), ExtSet([b1])), Sim(a2, b2) when a1 = a2 && b1 = b2 -> [(Inh(ExtSet([a1]), ExtSet([b1])), Some(beliefId(tv1, tv2)), None)]
                | _ -> []
            )
//     ((B & A_1..n) --> (A & A_1..n)), (B --> A)task("?", t) |- ((B & A_1..n) --> (A & A_1..n)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(ExtInt(b1::a1ton), ExtInt(a1::a1ton1)), Inh(b2, a2) when a1ton = a1ton1 && a1 = a2 && b1 = b2 -> [(Inh(ExtInt(b1::a1ton), ExtInt(a1::a1ton)), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     ((B | A_1..n) --> (A | A_1..n)), (B --> A)task("?", t) |- ((B | A_1..n) --> (A | A_1..n)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(IntInt(b1::a1ton), IntInt(a1::a1ton1)), Inh(b2, a2) when a1ton = a1ton1 && a1 = a2 && b1 = b2 -> [(Inh(IntInt(b1::a1ton), IntInt(a1::a1ton)), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     ((S - A) --> (S - B)), (B --> A)task("?", t) |- ((S - A) --> (S - B)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(ExtDif(s, a1), ExtDif(s1, b1)), Inh(b2, a2) when s = s1 && a1 = a2 && b1 = b2 -> [(Inh(ExtDif(s, a1), ExtDif(s, b1)), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     ((S ~ A) --> (S ~ B)), (B --> A)task("?", t) |- ((S ~ A) --> (S ~ B)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(IntDif(s, a1), IntDif(s1, b1)), Inh(b2, a2) when s = s1 && a1 = a2 && b1 = b2 -> [(Inh(IntDif(s, a1), IntDif(s, b1)), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     (W --> (B | A_1..n)), (W --> B)task("?", t) |- (W --> (B | A_1..n)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(w1, IntInt(b1::a1ton)), Inh(w2, b2) when b1 = b2 && w1 = w2 -> [(Inh(w1, IntInt(b1::a1ton)), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     ((B & A_1..n) --> W), (B --> W)task("?", t) |- ((B & A_1..n) --> W), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(ExtInt(b1::a1ton), w1), Inh(b2, w2) when b1 = b2 && w1 = w2 -> [(Inh(ExtInt(b1::a1ton), w1), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     (W --> (S - B)), (W --> B)task("?", t) |- (W --> (S - B)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(w1, ExtDif(s, b1)), Inh(w2, b2) when b1 = b2 && w1 = w2 -> [(Inh(w1, ExtDif(s, b1)), Some(beliefStructuralDif(tv1, tv2)), None)]
                | _ -> []
            )
//     ((S ~ B) --> W), (B --> W)task("?", t) |- ((S ~ B) --> W), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(IntDif(s, b1), w1), Inh(b2, w2) when b1 = b2 && w1 = w2 -> [(Inh(IntDif(s, b1), w1), Some(beliefStructuralDif(tv1, tv2)), None)]
                | _ -> []
            )
//     ((B * P) --> ?X), (B --> A)task("?", t) |- ((B * P) --> (A * P)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(Prod([b1; p]), QVar("X")), Inh(b2, a) when b1 = b2 -> [(Inh(Prod([b1; p]), Prod([a; p])), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     ((B * P) --> (A * P)), (B --> A)task("?", t) |- ((B * P) --> (A * P)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(Prod([b1; p]), Prod([a1; p1])), Inh(b2, a2) when p = p1 && a1 = a2 && b1 = b2 -> [(Inh(Prod([b1; p]), Prod([a1; p])), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     (\(N A _) --> ?X), (N --> R)task("?", t) |- (\(N A _) --> \(R A _)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(IntImg([n1; a; Constant "_"]), QVar("X")), Inh(n2, r) when n1 = n2 -> [(Inh(IntImg([n1; a; Constant "_"]), IntImg([r; a; Constant "_"])), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     (\(N A _) --> \(R A _)), (N --> R)task("?", t) |- (\(N A _) --> \(R A _)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(IntImg([n1; a; Constant "_"]), IntImg([r1; a1; Constant "_"])), Inh(n2, r2) when a = a1 && n1 = n2 && r1 = r2 -> [(Inh(IntImg([n1; a; Constant "_"]), IntImg([r1; a; Constant "_"])), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     (/(N _ B) --> ?X), (S --> B)task("?", t) |- (/(N _ B) --> /(N _ S)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(ExtImg([n; Constant "_"; b1]), QVar("X")), Inh(s, b2) when b1 = b2 -> [(Inh(ExtImg([n; Constant "_"; b1]), ExtImg([n; Constant "_"; s])), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     (/(N _ B) --> /(N _ S)), (S --> B)task("?", t) |- (/(N _ B) --> /(N _ S)), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(ExtImg([n; Constant "_"; b1]), ExtImg([n1; Constant "_"; s1])), Inh(s2, b2) when n = n1 && b1 = b2 && s1 = s2 -> [(Inh(ExtImg([n; Constant "_"; b1]), ExtImg([n; Constant "_"; s1])), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     --A, Atask("?", t) |- --A, (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Not(a1), a2 when a1 = a2 -> [(Not(a1), Some(beliefNeg(tv1, tv2)), None)]
                | _ -> []
            )
//     A, --Atask("?", t) |- A, (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | a1, Not(a2) when a1 = a2 -> [(a1, Some(beliefNeg(tv1, tv2)), None)]
                | _ -> []
            )
//     (B || A_1..n), Btask("?", t) |- (B || A_1..n), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Or(b1::a1ton), b2 when b1 = b2 -> [(Or(b1::a1ton), Some(beliefStrucuralDed(tv1, tv2)), None)]
                | _ -> []
            )
//     (S --> P), (S <-> P)task("?", t) |- (S --> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(s1, p1), Sim(s2, p2) when p1 = p2 && s1 = s2 -> [(Inh(s1, p1), Some(structuralInt(tv1)), None)]
                | _ -> []
            )
//     (S <-> P), (S --> P)task("?", t) |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Sim(s1, p1), Inh(s2, p2) when p1 = p2 && s1 = s2 -> [(Sim(s1, p1), Some(structuralAbd(tv1)), None)]
                | _ -> []
            )

//     (P --> S), (S --> P)task("?", t) |- (P --> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(p1, s1), Inh(s2, p2) when p1 = p2 && s1 = s2 -> [(Inh(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P --> S), (S --> P)task("?", t) |- (P --> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Inh(p1, s1), Inh(s2, p2) when p1 = p2 && s1 = s2 -> [(Inh(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P ==> S), (S ==> P)task("?", t) |- (P ==> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Imp(p1, s1), Imp(s2, p2) when p1 = p2 && s1 = s2 -> [(Imp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P ==> S), (S ==> P)task("?", t) |- (P ==> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | Imp(p1, s1), Imp(s2, p2) when p1 = p2 && s1 = s2 -> [(Imp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =|> S), (S =|> P)task("?", t) |- (P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | ConImp(p1, s1), ConImp(s2, p2) when p1 = p2 && s1 = s2 -> [(ConImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =|> S), (S =|> P)task("?", t) |- (P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | ConImp(p1, s1), ConImp(s2, p2) when p1 = p2 && s1 = s2 -> [(ConImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =|> S), (S =/> P)task("?", t) |- (P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | RetImp(p1, s1), PreImp(s2, p2) when p1 = p2 && s1 = s2 -> [(RetImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =|> S), (S =/> P)task("?", t) |- (P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | RetImp(p1, s1), PreImp(s2, p2) when p1 = p2 && s1 = s2 -> [(RetImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =/> S), (S =|> P)task("?", t) |- (P =/> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | PreImp(p1, s1), RetImp(s2, p2) when p1 = p2 && s1 = s2 -> [(PreImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
//     (P =/> S), (S =|> P)task("?", t) |- (P =/> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                match t.S.Key.Term, b.S.Key.Term with
                | PreImp(p1, s1), RetImp(s2, p2) when p1 = p2 && s1 = s2 -> [(PreImp(p1, s1), Some(cnv(tv1)), None)]
                | _ -> []
            )
    ]