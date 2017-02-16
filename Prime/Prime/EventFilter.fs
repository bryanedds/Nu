namespace Prime
open System
open System.ComponentModel
open System.Text.RegularExpressions

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
            Symbol.String (string rexpr, None) :> obj
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
            | Atom (pattern, _) | String (pattern, _) -> Rexpr pattern :> obj
            | Number (_, _) | Quote (_, _) | Symbols (_, _) -> failconv "Expected Symbol or String for conversion to Rexpr." ^ Some symbol
        | :? Rexpr -> source
        | _ -> failconv "Invalid RexprConverter conversion from source." None

/// Effectively new-types the Regex type to implement custom type-conversation without needing
/// explicit initialization by the client program.
and [<TypeConverter (typeof<RexprConverter>)>] Rexpr (pattern) =
    inherit Regex (pattern)

[<RequireQualifiedAccess>]
module EventFilter =

    /// Describes how events are filtered.
    [<Syntax
        ("Any NotAny All Pattern Empty", "", "", "",
         Constants.PrettyPrinter.DefaultThresholdMin,
         Constants.PrettyPrinter.DefaultThresholdMax)>]
    type [<NoEquality; NoComparison>] Filter =
        | All of Filter list
        | Any of Filter list
        | NotAny of Filter list
        | Pattern of Rexpr * Rexpr list
        | Empty

    /// Filter events.
    let rec filter addressStr (traceRev : EventInfo list) eventFilter =
        match eventFilter with
        | All exprs -> List.fold (fun passed eventFilter -> passed && filter addressStr traceRev eventFilter) true exprs
        | Any exprs -> List.fold (fun passed eventFilter -> passed || filter addressStr traceRev eventFilter) false exprs
        | NotAny exprs -> not (List.fold (fun passed eventFilter -> passed || filter addressStr traceRev eventFilter) false exprs)
        | Pattern (addressRexpr, traceRexpr) ->
            if addressRexpr.IsMatch addressStr then
                let mutable passes = true
                let mutable enr = enumerator traceRexpr
                for eventInfo in traceRev do
                    if passes && enr.MoveNext () then
                        passes <- enr.Current.IsMatch (scstring eventInfo)
                passes
            else false
        | Empty -> true