namespace Controller

open EventFunctions
open Parameters
open IController
open ControllerFunctions
open Providers

type ControllerClass() =
    // load module wide parameters from XML file or use defaults
    do ParameterLoader()

    // private API
//    let workCycle() = workCycle()
//    let unansweredQuestions = unansweredQuestions

    // public API
    interface IController with
        member x.Reset() = reset()
        member x.CancelExecute() = cancelExecute()
        member x.Execute() = execute()
        member x.Step() = step()
        member x.LoadNALFile filename = loadNALFile filename
        member x.ParseSentence text = parseSentence text
        member x.UpdateConceptTree() = updateConceptTree()
        member x.Cycle = SystemTime.Now

    member x.CommandResultBuffer with get() = commandBuffer

    // Expose the events as C# events
    [<CLIEvent>]
    member x.InferenceResult = InferenceResultEvent.Publish

    [<CLIEvent>]
    member x.ParseResult = ParseResultEvent.Publish

    [<CLIEvent>]
    member x.ParseError = ParseErrorEvent.Publish

    [<CLIEvent>]
    member x.StatusUpdate = StatusUpdateEvent.Publish

    [<CLIEvent>]
    member x.TaskCompleted = TaskCompletedEvent.Publish
        
    [<CLIEvent>]
    member x.InferenceAnswer = InferenceAnswerEvent.Publish

    [<CLIEvent>]
    member x.NewTasksResult = NewTasksEvent.Publish

    [<CLIEvent>]
    member x.ConceptCount = ConceptCountEvent.Publish

    [<CLIEvent>]
    member x.Threshold = ThresholdEvent.Publish

    [<CLIEvent>]
    member x.InferenceLength = InferenceLengthEvent.Publish

    [<CLIEvent>]
    member x.Answer = UserQuestionAnswerEvent.Publish

    [<CLIEvent>]
    member x.Terms = UpdateConceptTreeEvent.Publish

    [<CLIEvent>]
    member x.Metrics = UpdateMetricsEvent.Publish

