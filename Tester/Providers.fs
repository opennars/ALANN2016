module Providers

type Id() =
    static let _id              = ref 0
    static member Next()        = incr _id; !_id
    static member Reset()       = _id := 0

//    static let mutable _id      = -1L
//    static member Next()        = _id <- _id + 1L; _id
//    static member Reset()       = _id <- -1L


type SystemTime() =
    static let mutable _time    = 0L
    static member Tick()        = _time <- _time + 1L
    static member Now           = _time
    static member Reset()       = _time <- 0L