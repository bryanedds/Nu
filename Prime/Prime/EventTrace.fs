namespace Prime
open System

type [<CLIMutable; ReferenceEquality>] EventInfo =
    { ModuleName : string
      FunctionName : string
      MoreInfo : string }

// TODO: P1: consider replacing this with UList since we really want to add to the back anyway.
type EventTrace = EventInfo list

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EventInfo =

    let record moduleName functionName =
        { ModuleName = moduleName
          FunctionName = functionName
          MoreInfo = String.Empty }

    let record3 moduleName functionName moreInfo =
        { ModuleName = moduleName
          FunctionName = functionName
          MoreInfo = moreInfo }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EventTrace =

    let record moduleName functionName eventTrace : EventTrace =
        EventInfo.record moduleName functionName :: eventTrace

    let record4 moduleName functionName moreInfo eventTrace : EventTrace =
        EventInfo.record3 moduleName functionName moreInfo :: eventTrace

    let empty : EventTrace =
        []