module ControllerFunctions

open System.Collections.Generic
open System.Collections.ObjectModel
open System.Threading
open Akka.FSharp
open ALANN
open Types
open Parameters
open Providers
open Events
open EventFunctions
open FileParser
open SentenceParser
open TaskDispatcher
open TaskBuffer
open Messages
open CommandInjector
open ActiveConceptProcessor
open System.Diagnostics
open DerivedTaskBuffer

type ResultTask(question, answer, stamp) = 
    
    member val Question = TypeFormatters.TypeFormatter.Term question with get, set
    member val Answer = TypeFormatters.TypeFormatter.Sentence answer with get, set
    member val Stamp =TypeFormatters.TypeFormatter.Stamp stamp with get, set

let mutable cancelled = false
let mutable cancellationToken = new CancellationTokenSource() 

let commandBuffer = new ObservableCollection<ResultTask>()
let unansweredQuestions = List<Task>()
let sw = Stopwatch()

let updateResultBuffer(args : UserQuestionAnswerEventArgs) =
    let tryFindResultIndex (result : ResultTask) =
        commandBuffer |> Seq.tryFindIndex (fun r -> r.Question = result.Question)

    let result = ResultTask( args.Answer.S.Key.Term,
                             args.Answer.S.BestAnswer.Value.Sentence,
                             args.Answer.S.BestAnswer.Value.Stamp ) 

    match tryFindResultIndex result with
    | Some(i)
        -> commandBuffer.RemoveAt i
           commandBuffer.Insert(i, result)
    | None 
        -> commandBuffer.Add result

    if commandBuffer.Count > Parameters.COMMAND_BUFFER_MAX then
        commandBuffer.RemoveAt(0)


UserQuestionAnswerEvent.Publish.Add updateResultBuffer

let updateStatus(flag) =
    if SystemTime.Now % Parameters.STATUS_UPDATE_PERIOD = 0L then
        raiseStatusUpdateEvent("Cycle Time: " + SystemTime.Now.ToString())

let workCycle() = 
    fileParserRef <! BufferNextTask
    sentenceParserRef <! BufferNextTask
    commandInjectorRef <! BufferNextTask
//    derivedTaskBufferRef <! BufferNextTask
    Async.RunSynchronously(taskBufferRef <? DispatchBuffer) |> ignore

    sw.Start()
    Async.RunSynchronously(activeConceptProcessorRef <? ProcessConcepts) |> ignore
    let elapsed = int(sw.ElapsedMilliseconds)
    if(elapsed < Parameters.MS_PER_CYCLE) then
        Thread.Sleep(Parameters.MS_PER_CYCLE - elapsed)
    else
        printfn "Exceeded Concept Processing Time: %dms" elapsed
    sw.Reset()
    publish (UpdateAV) system.EventStream |> ignore
    
let step() = 
    cancelled <- false
    SystemTime.Tick()
    updateStatus(true)
    workCycle()

let clearBuffers() =
    commandBuffer.Clear()
    fileParserRef <! Reset
    sentenceParserRef <! Reset
    commandInjectorRef <! Reset
    taskBufferRef <! Reset
//    derivedTaskBufferRef <! Reset
    taskDispatcherRef <! Reset
    activeConceptProcessorRef <! Reset

let reset() = 
    SystemTime.Reset()
    Id.Reset()
    clearBuffers()
    Thread.Sleep(500)
    clearBuffers()

let cancelExecute() = cancellationToken.Cancel()

let execute() = 
    let loop() = 
        async {
            cancelled <- false
            
            while SystemTime.Now <> Parameters.CYCLES do
                SystemTime.Tick()
                updateStatus(false)
                workCycle()
            raiseTaskCompletedEvent("Finished at: " + SystemTime.Now.ToString() )
            }

    let cancelHandler(ex) =
            cancelled <- true
            raiseTaskCompletedEvent("Paused at: " +  SystemTime.Now.ToString() )
            cancellationToken <- new CancellationTokenSource()

    try
        let cancellableTask = Async.TryCancelled(loop(), cancelHandler)
        Async.Start(cancellableTask , cancellationToken.Token )
    with
    | ex -> printfn "Exception in Controller.Execute() %s" ex.Message

let loadNALFile filename = 
    fileParserRef <! ParseFile(filename)

let parseSentence text = 
    sentenceParserRef <! ParseText(text)

let updateConceptTree() = taskDispatcherRef <! UpdateConceptTree(200)

