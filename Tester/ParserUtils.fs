module ParserUtils

open System
open System.IO;
open Types
open Parser
open Utilities
open Factories
open Parameters
open Providers
open EventFunctions
open TypeFormatters

let linesOfFile filename =
    seq { use stream = File.OpenRead filename
          use reader = new StreamReader(stream)
          while not reader.EndOfStream do
              yield reader.ReadLine() }

let parseLine line = Parser(line)

let parseText(text : string) =
    text.Split('\n')
    |> Array.map parseLine

let parseTextToTasks(text : string) =
    parseText text
    |> Array.filter (Option.isSome)
    |> Array.map ( fun s -> 
        let task = makeNewTask s.Value
        raiseParseResultEvent (TypeFormatter.ShortTask task)
        task )
         
let parseFile(filename) =
    linesOfFile filename 
    |> Seq.map parseLine
    |> Seq.toList

let parseFileToTasks(filename) =
    let lines = parseFile(filename)
    lines
    |> List.filter Option.isSome
    |> List.map ( fun s -> 
        let task = makeNewTask s.Value
        raiseParseResultEvent (TypeFormatter.ShortTask task)
        task )


