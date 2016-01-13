// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Collections.Generic
open System.Diagnostics
open C5
open Helpers
open Types
open IStore
open TaskStore
open TypeFormatters
open NLog

let rand = Random()
let termGenerator n = Inh(Constant(n.ToString()), Constant((n + 1).ToString()))
let budgetGenerator() = {P = single(rand.NextDouble()); D = single(rand.NextDouble())}
let truthGenerator() =  (single(rand.NextDouble()), single(rand.NextDouble()))
let taskKeyGenerator n = {SentenceType = SentenceType.Judgement; Term = termGenerator n; Tense = Tense.Eternal}
let sentenceGenerator n = {Key = taskKeyGenerator n; TV = Some (truthGenerator()); DV = None ; BestAnswer = None}
let stampGenerator() = {Created = 0L; Occurs = 0L; SC = 3; Origin = Origin.User; Trail = []; Activations = 0}
let taskGenerator n = {AV = budgetGenerator(); S = sentenceGenerator n; Stamp = stampGenerator(); Router = []}


let logger = LogManager.GetCurrentClassLogger()

[<EntryPoint>]
let main argv = 
    let rand = Random()
    let n = 5000
    let tasks = [| for i in 1..n -> taskGenerator i|]
    let taskStore = TaskStore(n)

    logger.Debug (sprintf "generating %d records..." n)

    let sw = Stopwatch()
    sw.Start()

    for i in 0..n - 1 do
        taskStore.Insert(tasks.[i])

    logger.Debug (sprintf "in %d ms" sw.ElapsedMilliseconds)

    logger.Debug (sprintf "******")

//    seq{for t in  taskStore -> t}
//    |> Seq.iter (TypeFormatter.ShortTask >> (printfn "%s"))

    let m = 50
    let p = 500
    logger.Debug (sprintf "updating (%d x %d) records..." p m)

    let sw = Stopwatch()
    sw.Start()

    for i in 1..p do
        for j in 0..m - 1 do
            taskStore.Update(tasks.[rand.Next(n)])

    logger.Debug (sprintf "in %d ms" sw.ElapsedMilliseconds)

    logger.Debug (sprintf "******")

    let a = set [1]
    let b = set [1;2;]
    let c = a - b
    let d = Set.maxElement

//    seq{for t in  taskStore -> t}
//    |> Seq.iter (TypeFormatter.ShortTask >> (printfn "%s"))
//
//    taskStore.TryGetValue(taskGenerator 6) 
//    |> Option.iter (TypeFormatter.ShortTask >> (printfn " before update: %s"))
//
//    let testTask = taskGenerator 6
//
//    taskStore.Update(testTask)
//    
//    taskStore.TryGetValue(testTask) 
//    |> Option.iter (TypeFormatter.ShortTask >> (printfn " after update: %s"))
//
//    while taskStore.Count > 0 do
//        printfn "%s" (TypeFormatter.ShortTask (taskStore.DeleteMin()))

    Console.ReadLine() |> ignore
    0 // return an integer exit code

