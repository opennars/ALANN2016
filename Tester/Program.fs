// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Runtime.InteropServices
open Helpers
open Types
open Utilities
open ParserUtils
open System.Diagnostics

type Conc = {Id :int; Active : bool}
[<EntryPoint>]
let main argv = 

    let t1 = "{John} --> /(taller_than {Tom} _)"
    let t2 = "_ --> {_}"
    let t3 ="/(paternalgrandfather _ {Lexie}) --> ?"
    let tasks = parseTextToTasks t3
    let term = tasks.[0].S.Key.Term
//    getTaskTerms term |> List.iter (TypeFormatters.TypeFormatter.Term >> (printfn "%s"))

    printfn "%d" (Marshal.SizeOf(term))

//    let n = 1000000
//    let sw = Stopwatch()
//    let rand = new Random()
//    let r() = rand.Next(100) < 25
//    let concepts = [|for c in 0..n - 1 -> {Id = c; Active = r()}|]
//
//    sw.Start()
//    let count = 
//        concepts
//        |> Array.filter (fun c -> c.Active)
//        |> Array.length
//    printfn "Processed %d Actives from %d Concepts in %d" count n (sw.ElapsedMilliseconds)
    Console.ReadLine() |> ignore
    0