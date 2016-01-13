module TruthFunctions

open Parameters
open Types

//personality factor (to be moved to parameters)
let k = Parameters.HORIZON

// Extended boolean operators
let _and lst = lst |> List.fold (*) 1.0f

//let _and lst = lst |> List.fold (fun acc x -> acc * x) 1.0f
let _not a = 1.0f - a
let _or lst = 1.0f - (lst |> List.fold (fun acc x -> (1.0f - x) * acc) 1.0f)

// Evidence conversion functions
let w2c (w) = w / (w + k)
let c2w (c) = k * c / (1.0f - c)

// Local inference
let exp (f, c) = c * (f - 0.5f) + 0.5f
let dec (p, d) = ( p * (d -0.5f))
let rev ((f1, c1), (f2, c2)) = 
    let a = c2w(c1)
    let b = c2w(c2)
    let w = a + b
    ((a * f1 + b * f2) / w, w2c(w))

// Immediate inference
let neg (f : single, c) = ((1.0f - f, c), GeneralTruth)
let cnv (f : single, c) = ((1.0f, w2c(_and [f; c])), GeneralTruth)
let cnt (f : single, c) = ((0.0f, (1.0f - f) * c / ((1.0f - f) * c + k)), GeneralTruth)

// Strong syllogism
let ded ((f1 : single, c1), (f2, c2)) = ((f1 * f2, f1 * c1 * f2 * c2), GeneralTruth)
let ana ((f1 : single, c1), (f2, c2)) = ((f1 * f2, f2 * c1 * c2), GeneralTruth)
let res ((f1 : single, c1), (f2, c2)) = (((f1 *  f2), (f1 + f2 - f1 * f2) * c1 * c2), GeneralTruth)

// Weak syllogism

let abd ((f1 : single, c1), (f2, c2)) =
    let w = _and [f2; c1; c2]
    let c = w2c(w)
    ((f1, c), GeneralTruth)

let ind (tv1, tv2) = 
    let (abdTV, _) = abd (tv2, tv1)
    (abdTV, GeneralTruth)

let exe ((f1 : single, c1), (f2, c2)) = ((1.0f, w2c(_and [f1; f2; c1; c2])), GeneralTruth)

let com ((f1 : single, c1), (f2, c2)) = 
    let f0 = _or [f1; f2]
    let f = if f0 = 0.0f then 0.0f else (_and [f1; f2] / f0)
    ((f, w2c(_and [f0; c1; c2])), GeneralTruth)

// Term composition
let int ((f1 : single, c1), (f2, c2)) = ((( _and [f1; f2]), _and [c1; c2]), GeneralTruth)
let uni ((f1 : single, c1), (f2, c2)) = ((( _or [f1; f2]), _and [c1; c2]), GeneralTruth)
let dif ((f1 : single, c1), (f2, c2)) = ((( _and [f1; _not f2]), _and [c1; c2]), GeneralTruth)

let redCon (tv1, tv2) =
    let (negTV1, _) = neg(tv1)
    let (v, _) = int (negTV1, tv2)
    let (dedTV, _) = ded(v, (1.0f, 1.0f))
    neg(dedTV)

let anonAna  ((f, c), _) = 
    let (anaTV, _) = ana((f, c), (1.0f, 1.0f))
    (anaTV, GeneralTruth) // TODO *** (f, w2c(c))

// Term decomposition
let pnn ((f1 : single, c1), (f2, c2)) =
    let fn = _and [f1; _not f2] 
    ((_not fn, _and [fn; c1; c2]), GeneralTruth)

let npp ((f1 : single, c1), (f2, c2)) = 
    let f = _and [_not f1; f2]
    ((f, _and [f; c1; c2]), GeneralTruth)

let pnp ((f1 : single, c1), (f2, c2)) = 
    let f = _and [f1; _not f2]
    ((f, _and [f; c1; c2]), GeneralTruth)

let ppp ((f1 : single, c1), (f2, c2)) =
    let f = _and [f1; f2]
    ((f, _and [f; c1; c2]), GeneralTruth)

let nnn ((f1, c1), (f2, c2)) = 
    let fn = _and [1.0f - f1; 1.0f - f2]
    ((1.0f - fn, _and [fn; c1; c2]), GeneralTruth)

// Analytic truths
let identity ((f : single, c : single)) = ((f, c), StructuralTruth)

let structuralDed (tv) = 
    let (dedTV, _) = ded ((tv), (1.0f, 1.0f))
    (dedTV, StructuralTruth)

let structuralAbd (tv) = 
    let (abdTV, _) = abd ((tv), (1.0f, 1.0f))
    (abdTV, StructuralTruth)

let structuralInt (tv) = 
    let (intTV, _) = int ((tv), (1.0f, 1.0f))
    (intTV, StructuralTruth)

let beliefStructuralDed (_, tv) = 
    let (dedTV, _) = ded (tv, (1.0f, 1.0f))
    (dedTV, StructuralTruth)

let beliefNeg (_, tv) = 
    let (negTV, _) = neg (tv)
    (negTV, StructuralTruth)

let beliefId (_, tv) = (tv, StructuralTruth)

let beliefStructuralDif (_, tv) = 
    let ((f, c), _) = ded (tv, (1.0f, 1.0f))
    ((1.0f - f, c), StructuralTruth)

// Desire functions
let desireStrong ((f1 : single, c1), (f2, c2)) = ((_and [f1; f2], _and [c1; c2; f2]), GeneralTruth)
let desireWeak ((f1 : single, c1), (f2, c2)) = ((_and [f1; f2], _and [c1; c2; f2; 1.0f / (1.0f / k)]), GeneralTruth)
let desireDed ((f1 : single, c1), (f2, c2)) = ((_and [f1; f2], _and [c1; c2]), GeneralTruth)
let desireId (tv, _) = (tv, StructuralTruth)

let desireStructuralStrong (tv1) = 
    let (desireTV, _) = desireStrong (tv1, (1.0f, 1.0f))
    (desireTV, StructuralTruth)

let desireNeg (tv) = 
    let (negTV, _) = neg(tv)
    (negTV, GeneralTruth)

let desireInd ((f1 : single, c1), (f2, c2)) = 
    let w = _and [f2; c1; c2]
    ((f1, w / (w + k)), GeneralTruth)

