module TypeFormatters

open System.Text
open Types

//type OpCodes = | InhOp = 1 | SimOp = 2 | ExtSetOp = 3 | IntSetOp = 4 | ExtIntOp = 5 | IntIntOp = 6 | ExtDifOp = 7 | IntDifOp = 8
//               | ProdOp = 9 | ExtImgOp = 10 | IntImgOp = 11 | NotOp = 12 | AndOp = 13 | OrOp = 14 | ImpOp = 15 | EquOp = 16
//               | QVarOp = 17 | DVarOp = 18 | IVarOp = 19 | PreImpOp = 20 | ConImpOp = 21 | RetImpOp = 22
//               | ConEquOp = 23 | PreEquOp = 24 | ParOp = 25 | SeqOp = 26 | OPeratorOp = 27
//               | SeparatorOp = 28 | CloserOp = 29
                
type TypeFormatter() =
    static member Budget(av : Budget) = sprintf "[%.2f %.2f]" av.P av.D
    static member Truth(f, c) = sprintf "{%.2f %.2f}" f c
    static member Desire(p, d) = sprintf "{|%.2f %.2f|}" p d
    static member TruthOption = function | Some (f, c) -> TypeFormatter.Truth(f,c) | _ -> ""
    static member DesireOption = function | Some (p, d) -> TypeFormatter.Desire(d, p) | _ -> ""

    static member TermToUTF8(t : Term) = 
        let rec tp term =

            let separateList lst sep =
                match lst with
                | [] -> sep + sep   // *** TO DO *** find cause of empty list
                | _ -> List.reduce (fun x y -> x + sep + y) (List.map tp lst )

            match term with
            | Not(s) -> "--,"  + tp s + ")"
            | And(lst) -> "&&," + separateList (lst |> Set.toList) "," + ")"
            | Or(lst) -> "||," +  separateList (lst |> Set.toList) "," + ")"
            | Imp(s, p) -> "Imp," + tp s + "," + tp p + ")"
            | PreImp(s, p) -> "PreImp," + tp s + "," + tp p + ")"
            | ConImp(s, p) -> "ConImp," + tp s + ","+ tp p + ")"
            | RetImp(s, p) -> "RetImp," + tp s + "," + tp p + ")"
            | Equ(s, p) -> "Equ," + tp s + "," + tp p + ")"
            | ConEqu(s, p) -> "ConEqu," + tp s + ","+ tp p + ")"
            | PreEqu(s, p) -> "PreEqu," + tp s + "," + tp p + ")"
            | Inh(s, p) -> "Inh," + tp s + "," + tp p + ")"
            | Sim(s, p) -> "Sim," + tp s + "," + tp p + ")"
            | Operator(lst) -> "^," + separateList lst "," + ")"
            | ExtSet(set) -> "ExtSet," + separateList (set |> Set.toList) "," + ")"
            | IntSet(set) -> "IntSet," + separateList (set |> Set.toList) "," + ")"
            | ExtInt(set) -> "ExtInt," + separateList (set |> Set.toList) "," + ")"
            | IntInt(set) -> "IntInt," + separateList (set |> Set.toList) "," + ")"
            | ExtDif(a, b) -> "-," + tp a + "," + tp b + ")"
            | IntDif(a, b) -> "~," + tp a + "," + tp b + ")"
            | Prod(lst) -> "*," + separateList lst "," + ")"
            | Par(set) -> "Par," + separateList (set |> Set.toList) "," + ")"
            | Seq(lst) -> "Seq," + separateList lst "," + ")"
            | ExtImg(lst) -> "ExtImg," + separateList lst "," + ")"
            | IntImg(lst) -> "IntImg," + separateList lst "," + ")"
            | Constant(c) -> c
            | IVar(c) -> "IVar," + c + ")"
            | DVar(c) -> "DVar," + c + ")"
            | QVar(c) -> "QVar," + c + ")"

        tp t

    static member Term(t : Term) = 
        let rec tp term =

            let separateList lst sep =
                match lst with
                | [] -> sep + sep   // *** TO DO *** find cause of empty list
                | _ -> List.reduce (fun x y -> x + sep + y) (List.map tp lst )

            match term with
            | Not(s) -> "--" + tp s
            | And(lst) -> "(" + separateList (lst |> Set.toList) " && " + ")"
            | Or(lst) -> "(" + separateList (lst |> Set.toList) " || " + ")"
            | Imp(s, p) -> "(" + tp s + " ==> " + tp p + ")"
            | PreImp(s, p) -> "(" + tp s + " =/> " + tp p + ")"
            | ConImp(s, p) -> "(" + tp s + " =|> "+ tp p + ")"
            | RetImp(s, p) -> "(" + tp s + " =|> " + tp p + ")"
            | Equ(s, p) -> "(" + tp s + " <=> " + tp p + ")"
            | ConEqu(s, p) -> "(" + tp s + " <|> "+ tp p + ")"
            | PreEqu(s, p) -> "(" + tp s + " </> " + tp p + ")"
            | Inh(s, p) -> "(" + tp s + " --> " + tp p + ")"
            | Sim(s, p) -> "(" + tp s + " <-> " + tp p + ")"
            | Operator(lst) -> "^(, " + separateList lst " " + ")"
            | ExtSet(set) -> "{" + separateList (set |> Set.toList) " " + "}"
            | IntSet(set) -> "[" + separateList (set |> Set.toList) " " + "]"
            | ExtInt(set) -> "(" + separateList (set |> Set.toList) " & " + ")"
            | IntInt(set) -> "(" + separateList (set |> Set.toList) " | " + ")"
            | ExtDif(a, b) -> "(" + tp a + " - " + tp b + ")"
            | IntDif(a, b) -> "(" + tp a + " ~ " + tp b + ")"
            | Prod(lst) -> "(" + separateList lst " * " + ")"
            | Par(set) -> "(" + separateList (set |> Set.toList) "; " + ")"
            | Seq(lst) -> "(" + separateList lst ", " + ")"
            | ExtImg(lst) -> "/(" + separateList lst " " + ")"
            | IntImg(lst) -> "\\(" + separateList lst " " + ")"
            | Constant(c) -> c
            | IVar(c) -> "$" + c
            | DVar(c) -> "#" + c
            | QVar(c) -> "?" + c

        tp t

    static member Tense =
        function
        | Eternal ->  ""
        | Past    ->  ":\:"
        | Present ->  ":|:"
        | Future  ->  ":/:"
        | NoTense -> ""

    static member Origin =
        function
        | User          -> "User"
        | Derived       -> "Derived"
        | ParentModule  -> "ParentModule"
        | PeerModule    -> "PeerModule"
        | ChildModule   -> "ChildModule"

    static member Trail(trail : Trail) = if not(List.isEmpty trail) then "[" + List.reduce (fun x y -> x + " " + y) (List.map (fun x -> x.ToString()) trail) + "]" else "[]"

    static member Stamp(stamp : Stamp) = 
        sprintf "{Created %s; Occurs %s; SC %d; Origin %A; Trail %s}" 
            (stamp.Created.ToString())
            (stamp.Occurs.ToString())
            stamp.SC 
            (TypeFormatter.Origin stamp.Origin) 
            (TypeFormatter.Trail stamp.Trail)

    static member Sentence(sentence) = 
        match sentence with
        | { Key = { Tense = tense; Term = term } as key; TV = None; DV = None } when key.SentenceType = SentenceType.Question  -> sprintf "? %s %s"  (TypeFormatter.Tense tense)  (TypeFormatter.Term term)
        | { Key = { Tense = tense; Term = term } as key; TV = Some tv; DV = None } when key.SentenceType = SentenceType.Judgement  -> sprintf "%s %s %s" (TypeFormatter.Tense tense) (TypeFormatter.Term term) (TypeFormatter.Truth tv)
        | { Key = { Tense = tense; Term = term } as key; TV = Some tv; DV = Some dv } when key.SentenceType = SentenceType.Judgement  -> sprintf "%s %s %s" (TypeFormatter.Tense tense) (TypeFormatter.Term term) (TypeFormatter.Truth tv)
        | { Key = { Tense = tense; Term = term } as key; TV = None; DV = Some dv } when key.SentenceType = SentenceType.Judgement  -> sprintf "%s %s %s" (TypeFormatter.Tense tense) (TypeFormatter.Term term) (TypeFormatter.Desire dv)
        | { Key = { Tense = tense; Term = term } as key; TV = None; DV = Some dv } when key.SentenceType = SentenceType.Goal  -> sprintf "! %s %s"  (TypeFormatter.Term term) (TypeFormatter.Desire dv)
        | { Key = { Tense = tense; Term = term } as key; TV = None; DV = None } when key.SentenceType = SentenceType.Quest  -> sprintf "?? %s"    (TypeFormatter.Term term)

        | _ -> "Wrong"

    static member Task(task : Task) = sprintf "%s %s %s" (TypeFormatter.Budget task.AV) (TypeFormatter.Sentence task.S) (TypeFormatter.Stamp task.Stamp)

    static member ShortTask(task : Task) = sprintf "%s %s" (TypeFormatter.Budget task.AV) (TypeFormatter.Sentence task.S)
