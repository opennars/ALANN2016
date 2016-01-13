module Metrics

open System
open System.Collections.Generic
open Akka
open Akka.FSharp
open Akka.Actor
open ALANN
open Types
open Messages
open EventFunctions

type MetricMessage = | UpdateMetricMsg of Metric | RaiseUpdateEvent

type State = {Metrics : Dictionary<MetricType, Metric>}


let initMetrics() = 
    let list = 
        [ (MetricType.DispatchCount,      Metric(MetricType.DispatchCount,      "0"))
          (MetricType.ActiveConceptCount, Metric(MetricType.ActiveConceptCount, "0")) ] 

    System.Linq.Enumerable.ToDictionary(list, fst, snd) :> IDictionary<MetricType, Metric>

let metricsUpdater (state : State) (mailbox : Actor<MetricMessage>) = function
    | UpdateMetricMsg(metric) 
        -> match state.Metrics.TryGetValue metric.Name with
           | true, _ -> state.Metrics.[metric.Name] <- metric
           | false, _ -> state.Metrics.Add(metric.Name, metric)

    | RaiseUpdateEvent
        -> raiseUpdateMetricsEvent([| for m in state.Metrics.Values -> m|])
        
let metricsUpdaterRef = spawn system "metricsWorker" (actorOf2 (metricsUpdater ({Metrics = Dictionary<MetricType, Metric>(initMetrics()) })))

system.Scheduler.ScheduleTellRepeatedly(TimeSpan.FromSeconds(1.0), TimeSpan.FromSeconds(0.25), metricsUpdaterRef, RaiseUpdateEvent)
