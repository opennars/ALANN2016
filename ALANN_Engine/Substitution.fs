module Substitution 

open Types

let rec subst r x y =
    let rec substList lst x y =
        match lst with
        | [] -> []
        | hd::tl -> (subst hd x y) :: (substList tl x y)

    match r with
    | Not(s)             -> Not(subst s x y)
    | And(hd :: tl)      -> And((subst hd x y)::(substList tl x y ))
    | Or(hd :: tl)       -> Or((subst hd x y)::(substList tl x y ))
    | Imp(s, p)          -> Imp((subst s x y), (subst p x y))
    | PreImp(s, p)       -> PreImp((subst s x y), (subst p x y))
    | ConImp(s, p)       -> ConImp((subst s x y), (subst p x y))
    | RetImp(s, p)       -> RetImp((subst s x y), (subst p x y))
    | Equ(s, p)          -> Equ((subst s x y), (subst p x y))
    | ConEqu(s, p)       -> ConEqu((subst s x y), (subst p x y))
    | PreEqu(s, p)       -> PreEqu((subst s x y), (subst p x y))
    | Inh(s, p)          -> Inh((subst s x y), (subst p x y))
    | Sim(s, p)          -> Sim((subst s x y), (subst p x y))
    | Operator(hd :: tl) -> Operator((subst hd x y)::(substList tl x y ))
    | ExtSet(hd :: tl)   -> ExtSet((subst hd x y)::(substList tl x y ))
    | IntSet(hd :: tl)   -> IntSet((subst hd x y)::(substList tl x y ))
    | ExtInt(hd :: tl)   -> ExtInt((subst hd x y)::(substList tl x y ))
    | IntInt(hd :: tl)   -> IntInt((subst hd x y)::(substList tl x y ))
    | ExtDif(a, b)       -> ExtDif((subst a x y), (subst b x y))
    | IntDif(a, b)       -> IntDif((subst a x y), (subst b x y))
    | Prod(hd :: tl)     -> Prod((subst hd x y)::(substList tl x y ))
    | Par(hd :: tl)      -> Par((subst hd x y)::(substList tl x y ))
    | Seq(hd :: tl)      -> Seq((subst hd x y)::(substList tl x y ))
    | ExtImg(hd :: tl)   -> ExtImg((subst hd x y)::(substList tl x y ))
    | IntImg(hd :: tl)   -> IntImg((subst hd x y)::(substList tl x y ))
    | IVar(c) when r = x -> y
    | DVar(c) when r = x -> y
    | QVar(c) when r = x -> y
    | Constant(c) when r = x -> y
    | _        -> r
