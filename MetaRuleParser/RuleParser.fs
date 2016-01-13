module RuleParser



open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

let str s = pstring s
let pcomment = str "**" <|> str "//" >>. skipRestOfLine false
let ws = skipSepBy spaces pcomment

let strWS s = pstring s .>> ws

let ignoreLine : Parser<string>   = skipRestOfLine false |>> (fun _ -> "")
let pPremises   = ws >>. (strWS "/*") >>. (charsTillString "->" true 300)
let pConclusion = ws >>. restOfLine false
let pRule       = pipe2 pPremises pConclusion (fun x y -> sprintf "| %s-> [(%s)]" x y)
let pLine       = ws >>. ((attempt pRule) <|> ignoreLine)
let pProg       = ws >>. (sepBy pLine newline) .>> eof

let Parser (program:string) =
    match run pProg program with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, e, s) -> printfn "Failure: %s" errorMsg; []

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
