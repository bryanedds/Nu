namespace Prime
open System
open System.ComponentModel

type [<CLIMutable; ReferenceEquality>] EventInfo =
    { ModuleName : string
      FunctionName : string
      MoreInfo : string }

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