// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open ParserUtils

[<EntryPoint>]
let main argv = 
    
    parseFileToTasks(@"D:\Visual Studio 2015\Projects\ALANN\MetaRuleParser\MetaRules.txt")
//    parseFileToTasks(@"D:\Visual Studio 2015\Projects\ALANN\MetaRuleParser\SampleRule.txt")

    Console.ReadLine() |> ignore
    0 // return an integer exit code
