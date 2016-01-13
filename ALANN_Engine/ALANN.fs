module ALANN

open Akka.FSharp

let system = System.create "ALANN" (Configuration.defaultConfig())
