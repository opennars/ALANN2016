module Trail


open Types
open Parameters
open Providers

let rec merge p q =
    p @ q |> List.sortDescending

let capTrail tr =
    let len =  Parameters.TRAIL_LENGTH - 1
    if tr |> List.length > len then
        tr |> List.take len
    else
        tr

let makeTrail0 id        = [id]
let makeTrail1 id t      = id::capTrail(t.Stamp.Trail)
let makeTrail2 id b t    = id::capTrail(merge t.Stamp.Trail b.Stamp.Trail)

let nonOverlappingEvidence tr1 tr2 = Set.intersect (set tr1) (set tr2) |> Set.isEmpty
let nonOverlappingDerivations tr1 tr2 = Set.intersect (set tr1) (set tr2) |> Set.isEmpty

let merge2 tr1 tr2 = tr1 @ tr2 // List.distinct tr1 @ tr2
let makeDerivation2 b t = capTrail(merge2 t.Stamp.DerivationTrail b.Stamp.DerivationTrail)
let makeDerivation3 b t term = capTrail(merge2 (term::t.Stamp.DerivationTrail) b.Stamp.DerivationTrail)
