module ParserUtils

open System.Collections.Generic
open System.IO;
open RuleParser
open MetaTypes
open TypeFormatters
open Types
open System

let headerStr =
    """
module ForwardRules

open System
open Types
open TruthFunctions
open Utilities
open Reduce
open Substitution
open InferenceRuleHelpers

let forwardRules : RuleFunc list = 
    [
    """

let tailStr = 
    """
    ]
    """
let funcStr s =
    sprintf 
        "
    (fun ((t: Term, b: Term), (tv1 : Truth, tv2 : Truth), (dv1 : Truth, dv2 : Truth)) ->
           match t, b with
           %s
           | _ -> []
       )
        "
        s

let readFile(filename) =
    use stream = File.OpenRead filename
    use reader = new StreamReader(stream)
    reader.ReadToEnd()

let parseFileToTasks(filename) =
    printfn "%s" headerStr
    readFile(filename)
    |> Parser
    |> List.filter (fun s -> s <> "")
    |> List.iteri (fun i str -> printfn "// Rule %d\n\n%s" i (funcStr str))
    printfn "%s" tailStr