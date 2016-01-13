module Parameters

    open System.Xml.Linq
    open System.Collections.Generic

    type Parameters private() =
        static member val HORIZON                             = 1.0f with get, set        // System Personality Factor
        static member val CYCLES                              = 5000000L with get, set    // Max system cycles - set to 0L for infinite cycles
        static member val LATENCY_PERIOD                      = 0L with get, set          // Concept latency period in system cycles
        static member val MS_PER_CYCLE                        = 50 with get, set          // Milliseconds per cycle (77 cycles = 12 hz human alpha wave e.g. 1000 / 12)
        static member val DURATION                            = 5L with get, set          // Temporal Event duration window in cycles
        static member val ACTIVATION_THRESHOLD                = 0.95f with get, set       // Concept Activation Threshold - when threshold is above priority concept is activated
        static member val PRIME_ACTIVATION                    = 1.00f with get, set       // Amount of activation contributed by a primer
        static member val PRIME_ACTIVATIONS_MAX               = 5 with get, set           // The maximum number of primers a concept can send on Activation
        static member val INFERENCE_TASKS_PER_CYCLE           = 2 with get, set           // Number of tasks to select for each inference task
        static member val PRIME_THRESHOLD                     = 0.30f with get, set       // Amount of activation contributed by a primer
        static member val INFERENCE_THRESHOLD                 = 0.10f with get, set       // Minimum bellief priority to be accepted as active belief
        static member val CONCEPTS_ACTIVATIONS_FREQUENCY      = 10.0 with get, set        // The frequency of running ActiveConceptProcesssor in millisecs
        static member val INFERENCE_BELIEFS_PER_CYCLE         = 5 with get, set           // Number of beliefs to select for each inference task
        static member val CONFIDENCE                          = 0.9f with get, set        // Truth Value confidence component
        static member val FREQUENCY                           = 1.0f with get, set        // Truth Value frequency component
        static member val MINIMUM_CONFIDENCE                  = 0.3f with get, set        // don't accept inference results with confidence below this Value
        static member val STI                                 = 0.5f with get, set        // Short Term Importance default Value AKA priority
        static member val LTI                                 = 0.5f with get, set        // long Term Importance default Value AKA duration
        static member val USERSTI                             = 0.75f with get, set       // Short Term Importance default Value for user entered Values AKA priority
        static member val USERLTI                             = 0.75f with get, set       // long Term Importance default Value for user entered Values AKA duration
        static member val TRAIL_LENGTH                        = 15 with get, set          // maximum length allowed for inference trail within stamp
        static member val CONCEPT_SELECTION_FACTOR            = 1.0 with get, set         // Determines the attentional focus - the > the Value the < the selection range
        static member val TASK_SELECTION_FACTOR               = 1.0 with get, set         // Determines the attentional focus - the > the Value the < the selection range
        static member val BELIEF_SELECTION_FACTOR             = 1.0 with get, set         // Determines the attentional focus - the > the Value the < the selection range
        static member val NOVELTASK_SELECTION_FACTOR          = 5.0f with get, set        // Determines the attentional focus - the > the Value the < the selection range
        static member val COMMAND_BUFFER_MAX                  = 20 with get, set          // Maximum number of user entered comand tasks that are re-injected periodically
        static member val RAZOR_PARAMETER                     = 1.0f with get, set        // Syntactic simplicity adjustment parameter (e/n^r) 
        static member val DECISION_THRESHOLD                  = 0.5f with get, set        // Accepts goals above this threshold
        static member val CONCEPT_CAPACITY                    = 20000 with get, set       // Size of concept pool in Working memory
        static member val BELIEF_CAPACITY                     = 25 with get, set         // Size of Belief pool within each concept
        static member val TASK_CAPACITY                       = 2 with get, set           // Size of Task pool within each concept
        static member val INFERENCE_ACTORS                    = 30 with get, set          // Number of threads to run the inference step - one concept per thread
        static member val STATUS_UPDATE_PERIOD                = 1L with get, set          // Number of cycles to wait before updating the status bar
        static member val NEW_TASKS_PER_THREAD_MAX            = 30 with get, set          // Maximum number of new tasks per thread
        static member val NEW_TASKS_PER_CYCLE                 = 200 with get, set         // Maximum number of new tasks per system cycle
        static member val NOVEL_TASK_EXPECTATION_THRESHOLD    = 0.66f with get, set       // Threshold above which new tasks are accepted as being novel
        static member val INFERENCE_SEARCH_DEPTH              = 100.0f with get, set      // 1.0 to inf where 1.0 is shallow search, the deeper the value the deeper the search
        static member val DECAY_RATE                          = 0.30f with get, set       // Decay rate tuning parameter - smaller = slower decay rate (used in Attention.Decay)
        static member val TASKBAG_INSERTION_THRESHOLD         = 0.1f with get, set        // Minimium Durability value for insertion into task bag
        static member val BELIEFBAG_INSERTION_THRESHOLD       = 0.1f with get, set        // Minimium Durability value for insertion into belief bag

    let attr(node : XElement, name, defaultValue) =
        let attr = node.Attribute(XName.Get(name))
        if(not(isNull attr)) then attr.Value else defaultValue

    // Loads Default Parameters from XML file
    // Called only once in Controller as first task
    // XML file is called DefaultParameters.xml
    let ParameterLoader() =
        let parseParameters(node:XElement) =
            let f (a : XAttribute) =
                try
                    match a.Name.ToString(), a.Value with
                    | "Horizon", value                        -> Parameters.HORIZON                           <- float32(value)
                    | "LatencyPeriod", value                  -> Parameters.LATENCY_PERIOD                    <- int64(value)
                    | "Cycles", value                         -> Parameters.CYCLES                            <- int64(value)
                    | "Duration", value                       -> Parameters.DURATION                          <- int64(value)
                    | "TrailLength", value                    -> Parameters.TRAIL_LENGTH                      <- int(value)
                    | "ConceptCapacity", value                -> Parameters.CONCEPT_CAPACITY                  <- int(value) 
                    | "BeliefPoolCapacity", value             -> Parameters.BELIEF_CAPACITY                   <- int(value) 
                    | "TaskPoolCapacity", value               -> Parameters.TASK_CAPACITY                     <- int(value)
                    | "InferenceActors", value                -> Parameters.INFERENCE_ACTORS                  <- int(value) 
                    | "Confidence", value                     -> Parameters.CONFIDENCE                        <- float32(value)
                    | "Frequency", value                      -> Parameters.FREQUENCY                         <- float32(value)
                    | "MinimumConfidence", value              -> Parameters.MINIMUM_CONFIDENCE                <- float32(value)
                    | "STI", value                            -> Parameters.STI                               <- float32(value)
                    | "LTI", value                            -> Parameters.LTI                               <- float32(value)
                    | "UserSTI", value                        -> Parameters.USERSTI                           <- float32(value)
                    | "UserLTI", value                        -> Parameters.USERLTI                           <- float32(value)
                    | "ConceptSelectionFactor", value         -> Parameters.CONCEPT_SELECTION_FACTOR          <- float(value)
                    | "TaskSelectionFactor", value            -> Parameters.TASK_SELECTION_FACTOR             <- float(value)
                    | "BeliefSelectionFactor", value          -> Parameters.BELIEF_SELECTION_FACTOR           <- float(value)
                    | "NovelTaskSelectionFactor", value       -> Parameters.NOVELTASK_SELECTION_FACTOR        <- float32(value)
                    | "CommandBufferMax", value               -> Parameters.COMMAND_BUFFER_MAX                <- int32(value)
                    | "RazorParameter", value                 -> Parameters.RAZOR_PARAMETER                   <- float32(value)
                    | "DecisionThreshold", value              -> Parameters.DECISION_THRESHOLD                <- float32(value)
                    | "MillisecsPerCycle", value              -> Parameters.MS_PER_CYCLE                      <- int(value)
                    | "ActivationThreshold", value            -> Parameters.ACTIVATION_THRESHOLD              <- float32(value)
                    | "StatusUpdatePeriod", value             -> Parameters.STATUS_UPDATE_PERIOD              <- int64(value)
                    | "PrimeActivation", value                -> Parameters.PRIME_ACTIVATION                  <- float32(value)
                    | "InferenceThreshold", value             -> Parameters.INFERENCE_THRESHOLD               <- float32(value)
                    | "PrimeThreshold", value                 -> Parameters.PRIME_THRESHOLD                   <- float32(value)
                    | "InferenceTasksPerCycle", value         -> Parameters.INFERENCE_TASKS_PER_CYCLE         <- int(value)
                    | "PrimeActivationsMax", value            -> Parameters.PRIME_ACTIVATIONS_MAX             <- int(value)
                    | "InferenceBeliefsPerCycle", value       -> Parameters.INFERENCE_BELIEFS_PER_CYCLE       <- int(value)
                    | "ConceptActivationFrequency", value     -> Parameters.CONCEPTS_ACTIVATIONS_FREQUENCY    <- float(value)
                    | "NewTasksPerThreadMax", value           -> Parameters.NEW_TASKS_PER_THREAD_MAX          <- int(value)
                    | "NewTasksPerCycle", value               -> Parameters.NEW_TASKS_PER_CYCLE               <- int(value)
                    | "NovelTaskExpectationThreshold", value  -> Parameters.NOVEL_TASK_EXPECTATION_THRESHOLD  <- float32(value)
                    | "InferenceSearchDepth", value           -> Parameters.INFERENCE_SEARCH_DEPTH            <- float32(value)
                    | "DecayRate", value                      -> Parameters.DECAY_RATE                        <- float32(value)
                    | "TaskBagInsertionThreshold", value      -> Parameters.TASKBAG_INSERTION_THRESHOLD       <- float32(value)
                    | "BeliefBagInsertionThreshold", value    -> Parameters.BELIEFBAG_INSERTION_THRESHOLD     <- float32(value)

                    | _, _ -> failwith "Unknown Parameter"
                with
                | ex -> printfn "Exception in parseParameters: %A" ex.Message

            let attrs = node.Attributes() : IEnumerable<XAttribute>
            Seq.iter f attrs

        let loadParameters(node : XElement) =
            match node.Name.LocalName with
            | "Parameters" -> parseParameters node
            | name -> failwith ("Unknown Node: " + name)
        
        try
            loadParameters(XDocument.Load(@"D:/DefaultParameters.xml").Root)
        with
        | ex -> printfn "Error in LoadParameters: %A" ex.Message
