// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.ComponentModel
open System.Text.RegularExpressions
open Prime

/// Converts Rexpr types.
type RexprConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = typeof<Rexpr>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            string source :> obj
        elif destType = typeof<Symbol> then
            let rexpr = source :?> Rexpr
            Text (string rexpr, ValueNone) :> obj
        elif destType = typeof<Rexpr> then source
        else failconv "Invalid RexprConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Rexpr>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as str -> scvalue<Rexpr> str :> obj
        | :? Symbol as symbol ->
            match symbol with
            | Atom (pattern, _) | Text (pattern, _) -> Rexpr pattern :> obj
            | Number (_, _) | Quote (_, _) | Symbols (_, _) -> failconv "Expected Atom or Text for conversion to Rexpr." (Some symbol)
        | :? Rexpr -> source
        | _ -> failconv "Invalid RexprConverter conversion from source." None

/// Effectively new-types the Regex type to implement custom type-conversion without needing
/// explicit initialization by the client program.
and [<TypeConverter (typeof<RexprConverter>)>] Rexpr (pattern) =
    inherit Regex (pattern)
    member this.Pattern = pattern
    override this.Equals that =
        match that with
        | :? Rexpr as that -> strEq this.Pattern that.Pattern
        | _ -> false
    override this.GetHashCode () =
        hash pattern

/// Conveys debugging information about an event.
type [<CLIMutable>] EventInfo =
    { ModuleName : string
      FunctionName : string
      MoreInfo : string }

    /// Make event information.
    static member make moduleName functionName moreInfo =
        { ModuleName = moduleName
          FunctionName = functionName
          MoreInfo = moreInfo }

/// Describes how events are filtered.
type EventFilter =
    | All of EventFilter list
    | Any of EventFilter list
    | NotAny of EventFilter list
    | Pattern of Rexpr * Rexpr list
    | Pass

    /// Filter events.
    static member filter (addressStr : string) (traceRev : EventInfo list) eventFilter =
        match eventFilter with
        | All exprs -> List.fold (fun passed eventFilter -> passed && EventFilter.filter addressStr traceRev eventFilter) true exprs
        | Any exprs -> List.fold (fun passed eventFilter -> passed || EventFilter.filter addressStr traceRev eventFilter) false exprs
        | NotAny exprs -> not (List.fold (fun passed eventFilter -> passed || EventFilter.filter addressStr traceRev eventFilter) false exprs)
        | Pattern (addressRexpr, traceRexpr) ->
            if addressRexpr.IsMatch addressStr then
                let mutable passes = true
                let mutable enr = enumerator traceRexpr
                for eventInfo in traceRev do
                    if passes && enr.MoveNext () then
                        passes <- enr.Current.IsMatch (scstring eventInfo)
                passes
            else false
        | Pass -> true