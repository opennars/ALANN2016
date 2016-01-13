module RuleGenerator
//  [|
//     (fun (tv1, tv2 : Truth) -> function Inh(s1, p), Inh(s2, _) when s1 <> p && s1 = s2 -> [(Inh(p, s1), cnv(tv1))] | _ -> [])
//     ...
//  |]
open System.Collections.Generic
open MetaTypes
open TypeFormatters
open Types
open MetaTagConverter

type RuleGenerator() =
    static let separateList lst sep =
            match lst with
            | [] -> sep + sep   // *** TO DO *** find cause of empty list
            | _ -> List.reduce (fun x y -> x + sep + y) lst

    static let cMap = Dictionary<string, int>()
    static let renameConst c = 
        if cMap.ContainsKey(c) then cMap.[c] <- cMap.[c] + 1; c+cMap.[c].ToString() 
        else cMap.Add(c, 1); c
        
    static member Term(t : Term) = 
        let rec tp term = 

            let separateList lst sep =
                match lst with
                | [] -> sep + sep   // *** TO DO *** find cause of empty list
                | _ -> List.reduce (fun x y -> x + sep + y) (List.map tp lst )

            match term with
            | Not(s) -> "Not(" + tp s + ")"
            | And(lst) -> "And(set [" + separateList (lst |> Set.toList) "; " + "])"
            | Or(lst) -> "Or(set [" + separateList (lst |> Set.toList) "; " + "])"
            | Imp(s, p) -> "Imp(" + tp s + ", " + tp p + ")"
            | PreImp(s, p) -> "PreImp(" + tp s + ", " + tp p + ")"
            | ConImp(s, p) -> "ConImp(" + tp s + ", " + tp p + ")"
            | RetImp(s, p) -> "RetImp(" + tp s + ", " + tp p + ")"
            | Equ(s, p) -> "Equ(" + tp s + ", " + tp p + ")"
            | ConEqu(s, p) -> "ConEqu(" + tp s + ", " + tp p + ")"
            | PreEqu(s, p) -> "PreEqu(" + tp s + ", " + tp p + ")"
            | Sim(s, p) -> "Sim(" + tp s + ", " + tp p + ")"
            | Inh(s, p) -> "Inh(" + tp s + ", " + tp p + ")"
            | Operator(lst) -> "Operator([" + separateList lst "; " + "])"
            | ExtSet(set) -> "ExtSet([" + separateList (set |> Set.toList) "; " + "])"
            | IntSet(set) -> "IntSet([" + separateList (set |> Set.toList) "; " + "])"
            | ExtInt(set) -> "ExtInt(set [" + separateList (set |> Set.toList) "; " + "])"
            | IntInt(set) -> "IntInt(set [" + separateList (set |> Set.toList) "; " + "])"
            | ExtDif(a, b) -> "ExtDif(" + tp a + ", " + tp b + ")"
            | IntDif(a, b) -> "IntDif(" + tp a + ", " + tp b + ")"
            | Prod(lst) -> "Prod([" + separateList lst "; " + ")"
            | Par(set) -> "Par(set [" + separateList (set |> Set.toList) "; " + "])"
            | Seq(lst) -> "Seq([" + separateList lst "; " + "])"
            | ExtImg(lst) -> "ExtImg([" + separateList lst "; " + "])"
            | IntImg(lst) -> "IntImg([" + separateList lst "; " + "])"
            | Constant(c) -> (renameConst c).ToLower()
            | IVar(c) -> "$" + c
            | DVar(c) -> "#" + c
            | QVar(c) -> "?" + c

        tp t 
        
    static member BuildComment(rule) =
        let premises = rule.Premises
        let predicates = rule.Predicates
        let conclusions = rule.Conclusions

        let premise1 = fst premises
        let premise2 = snd premises

        let p1str = TypeFormatter.Term premise1
        let p2str = TypeFormatter.Term premise2

        let termsToStr terms =
            separateList (terms |> List.map TypeFormatter.Term) ","

        let predToStr pred =
            sprintf "%s(%s)" (fst pred) (termsToStr (snd pred))

        let predsToStr predicates =
            let result =
                predicates
                |> Option.map (List.map predToStr)

            match result with
            | Some result -> (separateList result ",")
            | None -> ""

        let conclusionTerm (conclusion : MetaConclusion) = 
            TypeFormatter.Term conclusion.Conclusion
            
        let predsStr = predsToStr predicates

        let metaDataToStr (metaData : MetaData) =
            sprintf "%s:%s" (fst metaData) (snd metaData)

        let metaDataListToStr (metaDataList : MetaData list) =
            separateList (metaDataList |> List.map metaDataToStr) ","

        let conclusionToStr (conclusion : MetaConclusion) = 
            sprintf "%s, %s" (conclusionTerm conclusion) (metaDataListToStr (snd conclusion))
            
        let conclusionsToStr conclusions =
            separateList (conclusions |> List.map conclusionToStr) "\n\r"

        let conclusionStr = conclusionsToStr conclusions

        let str = sprintf "%s, %s, %s |- %s" p1str p2str predsStr conclusionStr

        printfn "// %s" str

    static member BuildRule(rule) =
        let mutable allowBackward = false
        let mutable conclusionPremiseStr = ""
        let premises = rule.Premises
        let predicates = rule.Predicates
        let conclusions = rule.Conclusions

        let premise1 = fst premises
        let premise2 = snd premises

        let mutable p1str = RuleGenerator.Term premise1
        let mutable p2str = RuleGenerator.Term premise2
        cMap.Clear()
        printfn "cMap.Count = %d" cMap.Count

        let termsToStr terms =
            separateList (terms |> List.map RuleGenerator.Term) ","

        let predToStr pred =
            match pred with
            | ("not_equal", [s; p]) -> sprintf "%s <> %s" (RuleGenerator.Term s) (RuleGenerator.Term p)
            | (str, lst) -> sprintf "%s(%s)" str (separateList (lst |> List.map RuleGenerator.Term) ",")

        let predsToStr predicates =
            let result =
                predicates
                |> Option.map (List.map predToStr)

            match result with
            | Some result -> sprintf "when %s" (separateList result ",")
            | None -> ""

        let predsStr = predsToStr predicates

        let conclusionTermToStr (conclusion : MetaConclusion) = 
            RuleGenerator.Term (fst conclusion)
            
        let metaDataToStr (metaData : MetaData) =
            let key = (fst metaData) + (snd metaData)
            let metaFunc =MetaTagConverter.Lookup key
            match metaFunc with
            | "neg" | "cnv" | "cnt" -> sprintf "%s(tv)" metaFunc
            | "AllowBackward" -> allowBackward <- true; ""
            | _ -> sprintf "%s(tv1, tv2)" metaFunc

        let metaDataListToStr (metaDataList : MetaData list) =
            separateList (metaDataList |> List.map metaDataToStr) ","

        let conclusionToStr (conclusion : MetaConclusion) = 
            conclusionPremiseStr <- (conclusionTermToStr conclusion)
//            sprintf "%s, %s" (conclusionTermToStr conclusion) (metaDataListToStr (snd conclusion))
            sprintf "%s, %s" conclusionPremiseStr (metaDataListToStr (snd conclusion))
            
        let conclusionsToStr conclusions =
            separateList (conclusions |> List.map conclusionToStr) "\n\r"

        let conclusionStr = conclusionsToStr conclusions

    //    T, B |- C, [post] =>
    //          C, B, task("?") |- T, [post]
    //          C, T, task("?") |- B, [post]

        match allowBackward with
        | true -> 
            printfn "%s, %s %s -> [%s]" p1str p2str predsStr conclusionStr

            let tmpP1 = p1str
            let tmpP2 = p2str
            let tmpConl = conclusionPremiseStr

            p1str <- tmpConl
            conclusionPremiseStr <- tmpP1
            let conclusionStr = conclusionsToStr conclusions
            printfn "[DERIVED1]%s, %s %s -> [%s]" p1str p2str predsStr conclusionStr

            p1str <- tmpConl
            conclusionPremiseStr <- tmpP2
            let conclusionStr = conclusionsToStr conclusions
            printfn "[DERIVED2]%s, %s %s -> [%s]" p1str p2str predsStr conclusionStr
        | false -> ()

        let str = sprintf "%s, %s %s -> [%s]" p1str p2str predsStr conclusionStr

        printfn "%s" str

