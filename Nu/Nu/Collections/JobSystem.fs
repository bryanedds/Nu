namespace Nu
open System
open System.Collections.Concurrent
open System.Threading
open Prime
open Nu

/// The result of running a job.
type JobResult =
    | JobCompletion of DateTimeOffset * obj
    | JobException of DateTimeOffset * Exception
    member this.CompletionTime =
        match this with
        | JobCompletion (completionTime, _) -> completionTime
        | JobException (completionTime, _) -> completionTime

/// A job for threaded processing.
type Job =
    { JobId : obj
      Work : unit -> obj }

/// Processes jobs based on priority.
type JobSystem =
    
    /// Add a job for processing with the given priority (low number is higher priority).
    abstract Enqueue : single * Job -> unit

    /// Await the completion of a job with the given timeout.
    abstract TryAwait : TimeSpan * obj -> JobResult option

/// Processes jobs based on priority inline.
type JobSystemInline () =

    let jobResults = ConcurrentDictionary<obj, JobResult> ()

    interface JobSystem with

        /// Add a job for processing with the given priority (low number is higher priority).
        member this.Enqueue (_, job) =
            let result =
                try JobCompletion (DateTimeOffset.Now, job.Work ())
                with exn -> JobException (DateTimeOffset.Now, exn)
            jobResults.[job.JobId] <- result

        /// Await the completion of a job with the given timeout.
        member this.TryAwait (_, jobId) =
            match jobResults.TryRemove jobId with
            | (true, jobResult) -> Some jobResult
            | (false, _) -> None

/// Processes jobs based on priority in parallel.
type JobSystemParallel (resultExpirationTime : TimeSpan) =

    let executingRef = ref true
    let jobQueue = ConcurrentPriorityQueue<single, Job> ()
    let jobResults = ConcurrentDictionary<obj, JobResult> ()
    let _ =
        async {
            while lock executingRef (fun () -> executingRef.Value) do
                let mutable job = Unchecked.defaultof<_>
                if jobQueue.TryDequeue &job then
                    let work =
                        async {
                            let result =
                                try JobCompletion (DateTimeOffset.Now, job.Work ())
                                with exn -> JobException (DateTimeOffset.Now, exn)
                            jobResults.[job.JobId] <- result }
                    Async.Start work
                else
                    for entry in jobResults.ToArray () do
                        let expirationTime = DateTimeOffset.Now + resultExpirationTime
                        if entry.Value.CompletionTime > expirationTime then
                            match jobResults.TryRemove entry.Key with
                            | (true, jobResult) when jobResult.CompletionTime <> entry.Value.CompletionTime ->
                                jobResults.[entry.Key] <- jobResult // add back if not the one we intended to remove
                            | (_, _) -> ()
                    Async.Sleep 1 |> Async.RunSynchronously } |>
            Async.StartAsTask

    interface JobSystem with

        /// Add a job for processing with the given priority (low number is higher priority).
        member this.Enqueue (priority, job) =
            jobQueue.Enqueue (priority, job)

        /// Await the completion of a job with the given timeout.
        member this.TryAwait (timeOut, jobId) =
            let mutable now = DateTimeOffset.Now
            let timeOver = now + timeOut
            let mutable jobResultOpt = None
            let mutable timeOutExceeded = false
            while jobResultOpt.IsNone && not timeOutExceeded do
                match jobResults.TryRemove jobId with
                | (true, jobResult) -> jobResultOpt <- Some jobResult
                | (false, _) ->
                    if now > timeOver
                    then timeOutExceeded <- true
                    else Thread.Yield () |> ignore<bool>
                now <- DateTimeOffset.Now
            jobResultOpt