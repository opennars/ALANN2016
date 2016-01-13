module TypeFormatters

open Types
                
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
            | And(lst) -> "&&," + separateList lst "," + ")"
            | Or(lst) -> "||," +  separateList lst "," + ")"
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
            | ExtSet(lst) -> "ExtSet," + separateList lst "," + ")"
            | IntSet(lst) -> "IntSet," + separateList lst "," + ")"
            | ExtInt(lst) -> "ExtInt," + separateList lst "," + ")"
            | IntInt(lst) -> "IntInt," + separateList lst "," + ")"
            | ExtDif(a, b) -> "-," + tp a + "," + tp b + ")"
            | IntDif(a, b) -> "~," + tp a + "," + tp b + ")"
            | Prod(lst) -> "*," + separateList lst "," + ")"
            | Par(lst) -> "Par," + separateList lst "," + ")"
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
            | And(lst) -> "(" + separateList lst " && " + ")"
            | Or(lst) -> "(" + separateList lst " || " + ")"
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
            | ExtSet(lst) -> "{" + separateList lst " " + "}"
            | IntSet(lst) -> "[" + separateList lst " " + "]"
            | ExtInt(lst) -> "(" + separateList lst " & " + ")"
            | IntInt(lst) -> "(" + separateList lst " | " + ")"
            | ExtDif(a, b) -> "(" + tp a + " - " + tp b + ")"
            | IntDif(a, b) -> "(" + tp a + " ~ " + tp b + ")"
            | Prod(lst) -> "(" + separateList lst " * " + ")"
            | Par(lst) -> "(" + separateList lst "; " + ")"
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

    static member Sentence(sentence :  Sentence) = 
        let prefix s = 
            match s.Key.SentenceType with 
            | SentenceType.Question  -> "? "
            | SentenceType.Judgement -> ""
            | SentenceType.Goal      -> "! "
            | SentenceType.Quest     -> "?? "

        sprintf "%s %s %s %s %s"
                (prefix sentence) 
                (TypeFormatter.Tense sentence.Key.Tense)
                (TypeFormatter.Term sentence.Key.Term)
                (TypeFormatter.TruthOption sentence.TV)
                (TypeFormatter.DesireOption sentence.DV)

    static member Task(task : Task) = sprintf "%s %s %s" (TypeFormatter.Budget task.AV) (TypeFormatter.Sentence task.S) (TypeFormatter.Stamp task.Stamp)

    static member ShortTask(task : Task) = sprintf "%s %s" (TypeFormatter.Budget task.AV) (TypeFormatter.Sentence task.S)
