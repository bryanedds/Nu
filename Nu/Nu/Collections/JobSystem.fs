namespace Nu
open System
open System.Collections.Concurrent
open System.Threading.Tasks
open Prime
open Nu

/// The result of running a job.
type JobResult =
    | JobCompletion of obj
    | JobException of Exception
    | JobTimeout

/// A job for threaded processing.
type Job =
    { JobId : obj
      Work : unit -> JobResult }

[<RequireQualifiedAccess>]
module JobSystem =

    /// Processes jobs based on priority on the available threads.
    type JobSystem =
        private
            { ExecutingRef : bool ref
              JobQueue : ConcurrentPriorityQueue<single, Job>
              JobResults : ConcurrentDictionary<obj, JobResult>
              JobsProcessor : unit Task }

    /// Add a job for processing with the given priority (low number is higher priority).
    let enqueue jobPriority job jobSystem =
        jobSystem.JobQueue.Enqueue (jobPriority, job)

    /// Make (and start) a job processing system.
    let make () =
        let executingRef = ref true
        let jobQueue = ConcurrentPriorityQueue<single, Job> ()
        let jobResults = ConcurrentDictionary<obj, JobResult> ()
        let jobsProcessor =
            async {
                while lock executingRef (fun () -> executingRef.Value) do
                    let mutable job = Unchecked.defaultof<_>
                    if jobQueue.TryDequeue &job then
                        let work =
                            async {
                                let result =
                                    try JobCompletion (job.Work ())
                                    with exn -> JobException exn
                                if not (jobResults.TryAdd (job.JobId, result)) then
                                    Log.info ("Failed to add job result for job '" + scstring job.JobId + "' due to an already existing result.") }
                        Async.Start work
                    else 1 |> Async.Sleep |> Async.RunSynchronously }
        let jobSystem =
            { ExecutingRef = executingRef
              JobQueue = jobQueue
              JobResults = jobResults
              JobsProcessor = Async.StartAsTask jobsProcessor }
        jobSystem

    /// Await the completion of a job with the given timeout.
    let await timeOutOpt jobId jobSystem =
        let timeOverOpt = Option.map (fun timeOut -> DateTimeOffset.Now + timeOut) timeOutOpt
        let mutable jobResultOpt = None
        let mutable timeOut = false
        while jobResultOpt.IsNone && not timeOut do
            match jobSystem.JobResults.TryGetValue jobId with
            | (true, jobResult) -> jobResultOpt <- Some jobResult
            | (false, _) ->
                match timeOverOpt with
                | Some timeOver -> if timeOver > DateTimeOffset.Now then timeOut <- true
                | None -> ()
        match jobResultOpt with
        | Some jobResult -> jobResult
        | None when timeOut -> JobTimeout
        | None -> failwithumf ()

    /// Halt processing any jobs not yet in-flight.
    let cease jobSystem =
        lock jobSystem.ExecutingRef (fun () -> jobSystem.ExecutingRef.Value <- false)
        jobSystem.JobsProcessor.Result

/// Processes jobs based on priority on the available threads.
type JobSystem = JobSystem.JobSystem