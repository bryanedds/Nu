namespace Nu
open System
open System.Collections.Concurrent
open System.Threading.Tasks
open Prime
open Nu

type JobResult =
    | JobSuccess of obj
    | JobFailure of Exception
    | JobTimeout

type Job =
    { JobId : obj
      Work : unit -> JobResult }

[<RequireQualifiedAccess>]
module JobSystem =

    type JobSystem =
        private
            { JobQueue : ConcurrentPriorityQueue<single, Job>
              JobResults : ConcurrentDictionary<obj, JobResult>
              JobProcessor : unit Task
              ExecutingRef : bool ref }

    let enqueue jobPriority job jobSystem =
        jobSystem.JobQueue.Enqueue (jobPriority, job)

    let make () =
        let jobQueue = ConcurrentPriorityQueue<single, Job> ()
        let jobResults = ConcurrentDictionary<obj, JobResult> ()
        let executingRef = ref true
        let processor =
            async {
                while executingRef.Value do
                    if not jobQueue.IsEmpty then
                        let job = jobQueue.Dequeue ()
                        let work =
                            async {
                                let result =
                                    try JobSuccess (job.Work ())
                                    with exn -> JobFailure exn
                                if not (jobResults.TryAdd (job.JobId, result)) then
                                    Log.info ("Failed to add job result for job '" + scstring job.JobId + "' due to an already existing result.") }
                        Async.Start work
                    else 1 |> Async.Sleep |> Async.RunSynchronously }
        let jobSystem =
            { JobQueue = jobQueue
              JobResults = jobResults
              JobProcessor = Async.StartAsTask processor
              ExecutingRef = executingRef }
        jobSystem

    let await timeOutOpt jobId jobSystem =
        let timeOverOpt = Option.map (fun timeOut -> DateTimeOffset.UtcNow + timeOut) timeOutOpt
        let mutable jobResultOpt = None
        let mutable timeOut = false
        while jobResultOpt.IsNone && not timeOut do
            match jobSystem.JobResults.TryGetValue jobId with
            | (true, jobResult) -> jobResultOpt <- Some jobResult
            | (false, _) ->
                match timeOverOpt with
                | Some timeOver -> if timeOver > DateTimeOffset.UtcNow then timeOut <- true
                | None -> ()
        if timeOut
        then JobTimeout
        else jobResultOpt.Value

    let terminate jobSystem =
        jobSystem.ExecutingRef.Value <- false
        jobSystem.JobProcessor.Result

type JobSystem = JobSystem.JobSystem