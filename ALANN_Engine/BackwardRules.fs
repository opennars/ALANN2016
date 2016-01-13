module BackwardRules

open Types

let backwardRules : RuleFunc list = 
    [

    // *************************
    // Backward inference rules
    // *************************

//     (S --> P), (S <-> P)task("?", t) |- (S --> P), (NOT SHOWN)
//     (S <-> P), (S --> P)task("?", t) |- (S <-> P), (NOT SHOWN)
//     ({S} <-> {P}), {S} |- ({S} --> {P}), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(ExtSet([s1]), ExtSet([p])), ExtSet([s2]) when s1 = s2 -> [(Sim(ExtSet([s1]), ExtSet([p])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ExtSet([s1]), Inh(ExtSet([s2]), ExtSet([p])) when s1 = s2 -> [(Sim(ExtSet([s1]), ExtSet([p])), None, None)]
                    | _ -> []
                 )
//     ({S} <-> {P}), {P} |- ({S} --> {P}), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(ExtSet([s]), ExtSet([p1])), ExtSet([p2]) when p1 = p2 -> [(Sim(ExtSet([s]), ExtSet([p1])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ExtSet([p1]), Inh(ExtSet([s]), ExtSet([p2])) when p1 = p2 -> [(Sim(ExtSet([s]), ExtSet([p1])), None, None)]
                    | _ -> []
                 )
//     ([S] <-> [P]), [S] |- ([S] --> [P]), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(IntSet([s1]), IntSet([p])), IntSet([s2]) when s1 = s2 -> [(Sim(IntSet([s1]), IntSet([p])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | IntSet([s1]), Inh(IntSet([s2]), IntSet([p])) when s1 = s2 -> [(Sim(IntSet([s1]), IntSet([p])), None, None)]
                    | _ -> []
                 )
//     ([S] <-> [P]), [P] |- ([S] --> [P]), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(IntSet([s]), IntSet([p1])), IntSet([p2]) when p1 = p2 -> [(Sim(IntSet([s]), IntSet([p1])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | IntSet([p1]), Inh(IntSet([s]), IntSet([p2])) when p1 = p2 -> [(Sim(IntSet([s]), IntSet([p1])), None, None)]
                    | _ -> []
                 )
//     ({S} <-> {P}), {S} |- ({P} --> {S}), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(ExtSet([p]), ExtSet([s1])), ExtSet([s2]) when s1 = s2 -> [(Sim(ExtSet([s1]), ExtSet([p])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ExtSet([s1]), Inh(ExtSet([p]), ExtSet([s2])) when s1 = s2 -> [(Sim(ExtSet([s1]), ExtSet([p])), None, None)]
                    | _ -> []
                 )
//     ({S} <-> {P}), {P} |- ({P} --> {S}), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(ExtSet([p1]), ExtSet([s])), ExtSet([p2]) when p1 = p2 -> [(Sim(ExtSet([s]), ExtSet([p1])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ExtSet([p1]), Inh(ExtSet([p2]), ExtSet([s])) when p1 = p2 -> [(Sim(ExtSet([s]), ExtSet([p1])), None, None)]
                    | _ -> []
                 )
//     ([S] <-> [P]), [S] |- ([P] --> [S]), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(IntSet([p]), IntSet([s1])), IntSet([s2]) when s1 = s2 -> [(Sim(IntSet([s1]), IntSet([p])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | IntSet([s1]), Inh(IntSet([p]), IntSet([s2])) when s1 = s2 -> [(Sim(IntSet([s1]), IntSet([p])), None, None)]
                    | _ -> []
                 )
//     ([S] <-> [P]), [P] |- ([P] --> [S]), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(IntSet([p1]), IntSet([s])), IntSet([p2]) when p1 = p2 -> [(Sim(IntSet([s]), IntSet([p1])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | IntSet([p1]), Inh(IntSet([p2]), IntSet([s])) when p1 = p2 -> [(Sim(IntSet([s]), IntSet([p1])), None, None)]
                    | _ -> []
                 )
//     ({S} <-> {P}), {S} |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s1, p), ExtSet([s2]) when s1 = s2 -> [(Sim(ExtSet([s1]), ExtSet([p])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ExtSet([s1]), Sim(s2, p) when s1 = s2 -> [(Sim(ExtSet([s1]), ExtSet([p])), None, None)]
                    | _ -> []
                 )
//     ({S} <-> {P}), {P} |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, p1), ExtSet([p2]) when p1 = p2 -> [(Sim(ExtSet([s]), ExtSet([p1])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ExtSet([p1]), Sim(s, p2) when p1 = p2 -> [(Sim(ExtSet([s]), ExtSet([p1])), None, None)]
                    | _ -> []
                 )
//     ([S] <-> [P]), [S] |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s1, p), IntSet([s2]) when s1 = s2 -> [(Sim(IntSet([s1]), IntSet([p])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | IntSet([s1]), Sim(s2, p) when s1 = s2 -> [(Sim(IntSet([s1]), IntSet([p])), None, None)]
                    | _ -> []
                 )
//     ([S] <-> [P]), [P] |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, p1), IntSet([p2]) when p1 = p2 -> [(Sim(IntSet([s]), IntSet([p1])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | IntSet([p1]), Sim(s, p2) when p1 = p2 -> [(Sim(IntSet([s]), IntSet([p1])), None, None)]
                    | _ -> []
                 )
//     (S --> {P}), S |- (S <-> {P}), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s1, ExtSet([p])), s2 when s1 = s2 -> [(Inh(s1, ExtSet([p])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | s1, Sim(s2, ExtSet([p])) when s1 = s2 -> [(Inh(s1, ExtSet([p])), None, None)]
                    | _ -> []
                 )
//     (S --> {P}), {P} |- (S <-> {P}), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, ExtSet([p1])), ExtSet([p2]) when p1 = p2 -> [(Inh(s, ExtSet([p1])), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ExtSet([p1]), Sim(s, ExtSet([p2])) when p1 = p2 -> [(Inh(s, ExtSet([p1])), None, None)]
                    | _ -> []
                 )
//     ([S] --> P), [S] |- ([S] <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(IntSet([s1]), p), IntSet([s2]) when s1 = s2 -> [(Inh(IntSet([s1]), p), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | IntSet([s1]), Sim(IntSet([s2]), p) when s1 = s2 -> [(Inh(IntSet([s1]), p), None, None)]
                    | _ -> []
                 )
//     ([S] --> P), P |- ([S] <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(IntSet([s]), p1), p2 when p1 = p2 -> [(Inh(IntSet([s]), p1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | p1, Sim(IntSet([s]), p2) when p1 = p2 -> [(Inh(IntSet([s]), p1), None, None)]
                    | _ -> []
                 )
//     (--S ==> P), P |- (--P ==> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Imp(Not(p1), s), p2 when p1 = p2 -> [(Imp(Not(s), p1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | p1, Imp(Not(p2), s) when p1 = p2 -> [(Imp(Not(s), p1), None, None)]
                    | _ -> []
                 )
//     (--S ==> P), --S |- (--P ==> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Imp(Not(p), s1), Not(s2) when s1 = s2 -> [(Imp(Not(s1), p), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Not(s1), Imp(Not(p), s2) when s1 = s2 -> [(Imp(Not(s1), p), None, None)]
                    | _ -> []
                 )
//     (--S =|> P), P |- (--P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ConImp(Not(p1), s), p2 when p1 = p2 -> [(ConImp(Not(s), p1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | p1, ConImp(Not(p2), s) when p1 = p2 -> [(ConImp(Not(s), p1), None, None)]
                    | _ -> []
                 )
//     (--S =|> P), --S |- (--P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | ConImp(Not(p), s1), Not(s2) when s1 = s2 -> [(ConImp(Not(s1), p), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Not(s1), ConImp(Not(p), s2) when s1 = s2 -> [(ConImp(Not(s1), p), None, None)]
                    | _ -> []
                 )
//     (--S =/> P), P |- (--P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | RetImp(Not(p1), s), p2 when p1 = p2 -> [(PreImp(Not(s), p1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | p1, RetImp(Not(p2), s) when p1 = p2 -> [(PreImp(Not(s), p1), None, None)]
                    | _ -> []
                 )
//     (--S =/> P), --S |- (--P =|> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | RetImp(Not(p), s1), Not(s2) when s1 = s2 -> [(PreImp(Not(s1), p), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Not(s1), RetImp(Not(p), s2) when s1 = s2 -> [(PreImp(Not(s1), p), None, None)]
                    | _ -> []
                 )
//     (--S =|> P), P |- (--P =/> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | PreImp(Not(p1), s), p2 when p1 = p2 -> [(RetImp(Not(s), p1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | p1, PreImp(Not(p2), s) when p1 = p2 -> [(RetImp(Not(s), p1), None, None)]
                    | _ -> []
                 )
//     (--S =|> P), --S |- (--P =/> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | PreImp(Not(p), s1), Not(s2) when s1 = s2 -> [(RetImp(Not(s1), p), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Not(s1), PreImp(Not(p), s2) when s1 = s2 -> [(RetImp(Not(s1), p), None, None)]
                    | _ -> []
                 )
//     (A --> B), Atask(negative, t) |- --(A --> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Not(Inh(a1, b)), a2 when a1 = a2 -> [(Inh(a1, b), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | a1, Not(Inh(a2, b)) when a1 = a2 -> [(Inh(a1, b), None, None)]
                    | _ -> []
                 )
//     (A --> B), Btask(negative, t) |- --(A --> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Not(Inh(a, b1)), b2 when b1 = b2 -> [(Inh(a, b1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | b1, Not(Inh(a, b2)) when b1 = b2 -> [(Inh(a, b1), None, None)]
                    | _ -> []
                 )
//     --(A --> B), Atask(negative, t) |- (A --> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(a1, b), a2 when a1 = a2 -> [(Not(Inh(a1, b)), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | a1, Inh(a2, b) when a1 = a2 -> [(Not(Inh(a1, b)), None, None)]
                    | _ -> []
                 )
//     --(A --> B), Btask(negative, t) |- (A --> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(a, b1), b2 when b1 = b2 -> [(Not(Inh(a, b1)), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | b1, Inh(a, b2) when b1 = b2 -> [(Not(Inh(a, b1)), None, None)]
                    | _ -> []
                 )
//     (A <-> B), Atask(negative, t) |- --(A <-> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Not(Sim(a1, b)), a2 when a1 = a2 -> [(Sim(a1, b), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | a1, Not(Sim(a2, b)) when a1 = a2 -> [(Sim(a1, b), None, None)]
                    | _ -> []
                 )
//     (A <-> B), Btask(negative, t) |- --(A <-> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Not(Sim(a, b1)), b2 when b1 = b2 -> [(Sim(a, b1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | b1, Not(Sim(a, b2)) when b1 = b2 -> [(Sim(a, b1), None, None)]
                    | _ -> []
                 )
//     --(A <-> B), Atask(negative, t) |- (A <-> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(a1, b), a2 when a1 = a2 -> [(Not(Sim(a1, b)), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | a1, Sim(a2, b) when a1 = a2 -> [(Not(Sim(a1, b)), None, None)]
                    | _ -> []
                 )
//     --(A <-> B), Btask(negative, t) |- (A <-> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(a, b1), b2 when b1 = b2 -> [(Not(Sim(a, b1)), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | b1, Sim(a, b2) when b1 = b2 -> [(Not(Sim(a, b1)), None, None)]
                    | _ -> []
                 )
//     (A --> B), (B --> C)A <> C |- (A --> C), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(a, c), Inh(b, c1) when c = c1 && a <> b -> [(Inh(a, b), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(b, c), Inh(a, c1) when c = c1 && a <> b -> [(Inh(a, b), None, None)]
                    | _ -> []
                 )
//     (A --> B), (A --> C)B <> C |- (C --> B), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(c, b), Inh(a, c1) when c = c1 && a <> c -> [(Inh(a, b), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(a, c), Inh(c1, b) when c = c1 && a <> b -> [(Inh(a, b), None, None)]
                    | _ -> []
                 )
//     (A --> C), (B --> C)A <> B |- (B --> A), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(b, a), Inh(b1, c) when b = b1 && a <> c -> [(Inh(a, c), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(b, c), Inh(b1, a) when b = b1 && a <> c -> [(Inh(a, c), None, None)]
                    | _ -> []
                 )
//     (A --> B), (B --> C)C <> A |- (C --> A), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(c, a), Inh(b, c1) when c = c1 && a <> b -> [(Inh(a, b), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(b, c), Inh(c1, a) when c = c1 && a <> b -> [(Inh(a, b), None, None)]
                    | _ -> []
                 )
//     (S --> P), (P --> S) |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s1, p1), Inh(p2, s2) when p1 = p2 && s1 = s2 -> [(Inh(s1, p1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(p1, s1), Sim(s2, p2) when p1 = p2 && s1 = s2 -> [(Inh(s1, p1), None, None)]
                    | _ -> []
                 )
//     (S <-> P), (P --> S) |- (S --> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(s1, p1), Inh(p2, s2) when p1 = p2 && s1 = s2 -> [(Sim(s1, p1), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(p1, s1), Inh(s2, p2) when p1 = p2 && s1 = s2 -> [(Sim(s1, p1), None, None)]
                    | _ -> []
                 )
//     (P --> M), (S --> M)S <> P |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, p), Inh(s1, m) when s = s1 && m <> p -> [(Inh(p, m), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(s, m), Sim(s1, p) when s = s1 && m <> p -> [(Inh(p, m), None, None)]
                    | _ -> []
                 )
//     (M --> P), (M --> S)S <> P |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, p), Inh(m, s1) when s = s1 && m <> p -> [(Inh(m, p), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(m, s), Sim(s1, p) when s = s1 && m <> p -> [(Inh(m, p), None, None)]
                    | _ -> []
                 )
//     (M --> P), (S <-> M)S <> P |- (S --> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(s, p), Sim(s1, m) when s = s1 && m <> p -> [(Inh(m, p), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, m), Inh(s1, p) when s = s1 && m <> p -> [(Inh(m, p), None, None)]
                    | _ -> []
                 )
//     (P --> M), (S <-> M)S <> P |- (P --> S), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Inh(p, s), Sim(s1, m) when s = s1 && m <> p -> [(Inh(p, m), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, m1), Inh(p, s1) when s = s1 && m1 <> p -> [(Inh(p, m1), None, None)]
                    | _ -> []
                 )
//     (M <-> P), (S <-> M)S <> P |- (S <-> P), (NOT SHOWN)
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, p), Sim(s1, m1) when s = s1 && m1 <> p -> [(Sim(m1, p), None, None)]
                    | _ -> []
                 )
    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) ->
                    match t.S.Key.Term, b.S.Key.Term with
                    | Sim(s, m1), Sim(s1, p) when s = s1 && m1 <> p -> [(Sim(m1, p), None, None)]
                    | _ -> []
                 )
    ]