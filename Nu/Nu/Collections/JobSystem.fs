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

type JobDescription =
    { JobId : JobId
      Issuance : DateTimeOffset
      Work : unit -> JobResult }

type JobQueue =
    ConcurrentPriorityQueue<JobPriority, JobDescription>

type JobResults =
    ConcurrentDictionary<JobId, JobResult>

type JobQueryResult =
    | JobResult of JobResult
    | JobTimeout

type JobQuery =
    DateTimeOffset -> JobId -> JobQueryResult