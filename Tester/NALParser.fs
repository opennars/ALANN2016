module  Parser

open System
open System.Text
open System.Collections.Generic
open FParsec
open Types
open Reduce
open Utilities
open Factories
open Events
open EventFunctions

let vMap = new Dictionary<Term, int>()
let mutable vNum = 0
let pterm, ptermRef = createParserForwardedToRef<Term, unit>() 
let pstatement, pstatementRef = createParserForwardedToRef<Term, unit>()

let str s = pstring s
let pcomment = str "**" >>. skipRestOfLine true
let ws = skipSepBy spaces pcomment

let strWS s = pstring s .>> ws

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
        (ws >>. many1 (pElement .>> ws) |>> f)

let setBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
        (ws >>. many1 (pElement .>> ws) |>> f)

let pfloat_ws = pfloat .>> ws
let pstringliteral =
    let isIdentifierFirstChar c = isLetter c || c = '_' || isDigit c
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2 isIdentifierFirstChar isIdentifierChar 
    |>> fun i -> Constant(i)

let pstringliteral_ws = pstringliteral .>> ws 
let pconstant  = pstringliteral_ws
let pConstant = pconstant
let renameVar v = if vMap.ContainsKey(v) then (vMap.[v]).ToString() else vNum <- vNum + 1; vMap.Add(v, vNum); vNum.ToString()
let pivar = strWS "$" >>. pconstant |>> (renameVar >> IVar)
let pdvar = strWS "#" >>. pconstant |>> (renameVar >> DVar)
let pqvar = strWS "?" >>. opt pconstant |>> fun v -> QVar(if v.IsSome then renameVar v.Value else "")
let pvariable = choice [pivar; pdvar; pqvar]

let pextset = setBetweenStrings "{" "}" pterm ExtSet
let pextset_ws = pextset .>> ws
let pintset = setBetweenStrings "[" "]" pterm IntSet
let pintset_ws = pintset .>> ws
let pset = pextset_ws <|> pintset_ws

let parenthesised_term = between (strWS "(") (strWS ")") pstatement .>> ws
let pint_image = strWS "\\" >>. listBetweenStrings "(" ")" pterm IntImg
let pint_image_ws = pint_image .>> ws
let pext_image = strWS "/" >>. listBetweenStrings "(" ")" pterm ExtImg
let pext_image_ws = pext_image .>> ws
let poperator = strWS "^" >>. listBetweenStrings "(" ")" pterm Operator
let poperator_ws = poperator .>> ws
let pprefix_term = pint_image_ws <|> pext_image_ws <|> poperator_ws

let opp = new OperatorPrecedenceParser<Term, unit, unit>()
let statement = opp.ExpressionParser
opp.TermParser <- choice [pConstant; pvariable; pset; parenthesised_term; pprefix_term]
opp.AddOperator(InfixOperator   ("&&",   ws, 3, Associativity.Left, fun x y -> reduce(And( [x; y]))))
opp.AddOperator(InfixOperator   ("||",   ws, 3, Associativity.Left, fun x y -> reduce(Or( [x; y]))))
opp.AddOperator(InfixOperator   (",",    ws, 3, Associativity.Left, fun x y -> Seq([x; y])))
opp.AddOperator(InfixOperator   (";",    ws, 3, Associativity.Left, fun x y -> Par( [x; y])))
opp.AddOperator(InfixOperator   ("&",    ws, 2, Associativity.Left, fun a b -> reduce(ExtInt((sort [a; b])))))
opp.AddOperator(InfixOperator   ("|",    ws, 2, Associativity.Left, fun a b -> reduce(IntInt((sort [a; b])))))
opp.AddOperator(InfixOperator   ("*",    ws, 2, Associativity.Left, fun a b -> Prod([a; b])))
opp.AddOperator(InfixOperator   ("^",    ws, 2, Associativity.Left, fun a b -> Operator([a; b])))
opp.AddOperator(InfixOperator   ("-",    ws, 2, Associativity.Left, fun a b -> reduce(ExtDif(a, b))))
opp.AddOperator(InfixOperator   ("~",    ws, 2, Associativity.Left, fun a b -> reduce(IntDif(a, b))))
opp.AddOperator(PrefixOperator  ("--",   ws, 2, false, fun x -> Not(x)))
opp.AddOperator(InfixOperator   ("-->",  ws, 4, Associativity.Right, fun a b -> Inh(a, b)))
opp.AddOperator(InfixOperator   ("<->",  ws, 4, Associativity.Right, fun a b -> reduce(Sim(a, b))))
opp.AddOperator(InfixOperator   ("{--",  ws, 4, Associativity.Right, fun a b -> Inh(ExtSet(([a])), b)))
opp.AddOperator(InfixOperator   ("--]",  ws, 4, Associativity.Right, fun a b -> Inh(a, IntSet(([b])))))
opp.AddOperator(InfixOperator   ("{-]",  ws, 4, Associativity.Right, fun a b -> Inh(ExtSet(([a])), IntSet(([b])))))
opp.AddOperator(InfixOperator   ("==>",  ws, 5, Associativity.Right, fun a b -> Imp(a, b)))
opp.AddOperator(InfixOperator   ("<=>",  ws, 5, Associativity.Right, fun a b -> Equ(a, b)))
opp.AddOperator(InfixOperator   ("<|>",  ws, 5, Associativity.Right, fun a b -> ConEqu(a, b)))
opp.AddOperator(InfixOperator   ("</>",  ws, 5, Associativity.Right, fun a b -> PreEqu(a, b)))
opp.AddOperator(InfixOperator   ("=/>",  ws, 5, Associativity.Right, fun a b -> PreImp(a, b)))
opp.AddOperator(InfixOperator   ("=|>",  ws, 5, Associativity.Right, fun a b -> ConImp(a, b)))
opp.AddOperator(InfixOperator   ("=\\>", ws, 5, Associativity.Right, fun a b -> RetImp(a, b)))

do ptermRef      := choice [pConstant; pvariable; pset; parenthesised_term]
do pstatementRef := choice [statement]
let ptruth       = between (strWS "{") (strWS "}") (tuple2 pfloat_ws pfloat_ws) |>> fun (f, c) -> (single(f), single(c))
let pdesire      = between (strWS "{") (strWS "}") (tuple2 pfloat_ws pfloat_ws) |>> fun (p, d) -> (single(p), single(d))
let ptense       = (strWS ":|:" |>> fun t -> Present) <|> (strWS ":\:" |>> fun t -> Past) <|> (strWS ":/:" |>> fun t -> Future)
let pjudgement   = pipe3 (opt ptense) statement (opt ptruth)  (fun t s tv -> makeSentence(makeTaskKey(SentenceType.Judgement, s, (if Option.isNone t then Tense.Eternal else t.Value)), (if Option.isNone tv then Some (1.0f, 0.9f) else tv), None))
let pquestion    = pipe3 (strWS "?") (opt ptense) statement (fun _ t s -> makeSentence(makeTaskKey(SentenceType.Question, s, if Option.isNone t then Tense.NoTense else t.Value), None, None))
let pgoal        = pipe3 (strWS "!") statement (opt pdesire) (fun _ s dv -> makeSentence(makeTaskKey(SentenceType.Goal, s, Tense.NoTense), None, (if Option.isNone dv then Some (1.0f, 0.9f) else dv)))
let pquest       = pipe2 (strWS "??") statement (fun _ s -> makeSentence(makeTaskKey(SentenceType.Quest, s, Tense.NoTense), None, None))
let psentence    = ws >>. opt (choice [pquestion; pgoal; pjudgement; pquest]) .>> eof

exception ParseErrorException of string

let Parser(program:string) =
    vNum <- 0; vMap.Clear(); 
    match run psentence program with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, e, s) -> 
        raiseParseErrorEvent(errorMsg)
        None
