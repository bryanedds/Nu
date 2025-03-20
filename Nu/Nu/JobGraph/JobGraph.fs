﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Concurrent
open System.Threading
open Prime
open Nu

/// A job for threaded processing.
type Job =
    { JobId : obj
      IssueTime : DateTimeOffset
      Work : unit -> obj }
    static member make jobId work =
        { JobId = jobId
          IssueTime = DateTimeOffset.Now
          Work = work }

/// The result of running a job.
type JobResult =
    | JobCompletion of IssueTime : DateTimeOffset * ResultTime : DateTimeOffset * Result : obj
    | JobException of IssueTime : DateTimeOffset * ResultTime : DateTimeOffset * Exception : Exception
    member this.IssueTime =
        match this with
        | JobCompletion (issueTime, _, _) -> issueTime
        | JobException (issueTime, _, _) -> issueTime
    member this.ResultTime =
        match this with
        | JobCompletion (_, resultTime, _) -> resultTime
        | JobException (_, resultTime, _) -> resultTime

/// Processes jobs based on priority.
type JobGraph =

    /// Add a job for processing with the given priority (low number is higher priority).
    abstract Enqueue : priority : single * job : Job -> unit

    /// Await the completion of a job with the given timeout.
    /// Order of jobs with the same key is not guaranteed.
    abstract TryAwait : deadLine : DateTimeOffset * jobId : obj -> JobResult option

    /// Terminate job processing gracefully.
    abstract CleanUp : unit -> unit

/// Processes jobs based on priority inline.
type JobGraphInline () =

    let jobResults = ConcurrentDictionary<obj, JobResult> ()

    interface JobGraph with

        /// Add a job for processing with the given priority (low number is higher priority).
        member this.Enqueue (_, job) =
            let result =
                try JobCompletion (job.IssueTime, DateTimeOffset.Now, job.Work ())
                with exn -> JobException (job.IssueTime, DateTimeOffset.Now, exn)
            jobResults.AddOrUpdate (job.JobId, result, fun _ result' -> if result.IssueTime >= result'.IssueTime then result else result') |> ignore<JobResult>

        /// Await the completion of a job with the given timeout.
        /// Order of jobs with the same key is guaranteed.
        member this.TryAwait (_, jobId) =
            match jobResults.TryRemove jobId with
            | (true, jobResult) -> Some jobResult
            | (false, _) -> None

        /// Clean-up.
        member this.CleanUp () =
            jobResults.Clear ()

/// Processes jobs based on priority in parallel.
type JobGraphParallel (resultExpirationTime : TimeSpan) =

    let executingRef = ref true
    let jobQueue = ConcurrentPriorityQueue<single, Job> ()
    let jobResults = ConcurrentDictionary<obj, JobResult> ()
    let task =
        async {
            while lock executingRef (fun () -> executingRef.Value) do
                let mutable job = Unchecked.defaultof<_>
                if jobQueue.TryDequeue &job then
                    let work =
                        vsync {
                            let result =
                                try JobCompletion (job.IssueTime, DateTimeOffset.Now, job.Work ())
                                with exn -> JobException (job.IssueTime, DateTimeOffset.Now, exn)
                            jobResults.AddOrUpdate (job.JobId, result, fun _ existing -> if result.IssueTime >= existing.IssueTime then result else existing) |> ignore<JobResult> }
                    Vsync.Start work
                else
                    let now = DateTimeOffset.Now
                    for entry in jobResults.ToArray () do
                        if now > entry.Value.ResultTime + resultExpirationTime then
                            match jobResults.TryRemove entry.Key with // we add it back if not the one we intended to remove
                            | (true, jobResult) when now <= jobResult.ResultTime + resultExpirationTime ->
                                jobResults.AddOrUpdate (entry.Key, jobResult, fun _ existing -> if jobResult.IssueTime >= existing.IssueTime then jobResult else existing) |> ignore<JobResult>
                            | (_, _) -> ()
                    1 |> Async.Sleep |> Async.RunSynchronously } |>
            Async.StartAsTask

    interface JobGraph with

        /// Add a job for processing with the given priority (low number is higher priority).
        member this.Enqueue (priority, job) =
            jobQueue.Enqueue (priority, job)

        /// Await the completion of a job with the given timeout.
        /// Order of jobs with the same key is not guaranteed.
        member this.TryAwait (deadline : DateTimeOffset, jobId : obj) =
            let mutable jobResultOpt = None
            let mutable deadlinePassed = false
            while jobResultOpt.IsNone && not deadlinePassed do
                match jobResults.TryRemove jobId with
                | (true, jobResult) -> jobResultOpt <- Some jobResult
                | (false, _) ->
                    if DateTimeOffset.Now > deadline
                    then deadlinePassed <- true
                    else Thread.Yield () |> ignore<bool>
            jobResultOpt

        /// Terminate job processing gracefully.
        member this.CleanUp () =
            lock executingRef $ fun () ->
                executingRef.Value <- false
            task.Wait ()
            let mutable unused = Unchecked.defaultof<Job>
            while jobQueue.TryDequeue &unused do ()
            jobResults.Clear ()