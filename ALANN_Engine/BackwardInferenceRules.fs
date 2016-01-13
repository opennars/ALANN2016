module BackwardInferenceRules

open Types

let backwardRules = 
    [|
    (fun (tv1 : Truth, tv2 : Truth) -> function Inh(a, c1), Inh(b, c2) when a <> b && c1 = c2 -> [(Inh(a, b), (0.0f, 0.0f))] | _ -> [])
    (fun (tv1 : Truth, tv2 : Truth) -> function Inh(a1, c), Inh(a2, b) when b <> c && a1 = a2 -> [(Inh(b, c), (0.0f, 0.0f))] | _ -> [])
    |]