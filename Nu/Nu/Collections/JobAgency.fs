namespace Nu
open System
open System.Collections.Concurrent
open Prime
open Nu

type JobId =
    obj

type JobPriority =
    single

type JobResult =
    | JobSuccess of DateTimeOffset * obj
    | JobFailure of DateTimeOffset * Exception

type Job =
    { JobId : JobId
      Issuance : DateTimeOffset
      Work : unit -> JobResult }

type JobQueue =
    ConcurrentPriorityQueue<JobPriority, Job>

type JobResults =
    ConcurrentDictionary<JobId, JobResult>

type JobQueryResult =
    | JobResult of JobResult
    | JobTimeout

type JobQuery =
    TimeSpan -> JobId -> JobQueryResult