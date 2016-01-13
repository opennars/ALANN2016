namespace IController

type IConcept =
    abstract Name : string with get, set

type IController =
    abstract Reset : unit -> unit
    abstract Execute : unit -> unit
    abstract CancelExecute : unit -> unit
    abstract Step : unit -> unit
    abstract LoadNALFile : string -> unit
    abstract ParseSentence : string -> unit
    abstract UpdateConceptTree : unit -> unit
    abstract Cycle : int64 with get
    
