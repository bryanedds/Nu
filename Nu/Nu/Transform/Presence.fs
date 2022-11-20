namespace Nu
open System
open Prime
open Nu

/// Describes the form of an element's presence.
/// TODO: see if implemented IEquatable will keep an expression like `pres = pres2` from boxing. If so, consolidate the
/// eq and neq functions below into the definition. This would need to be done for our other user-defined value types
/// as well.
[<Syntax
    ("Enclosed Exposed Imposter Prominent Omnipresent", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] Presence =
    /// Inside an enclosed structure so you have to be close to see them.
    | Enclosed
    /// Outside an enclosed structure so visible from a distance.
    | Exposed
    /// Visible for miles but invisible once as close as Exposed or Enclosed.
    | Imposter
    /// Both Exposed and Imposter.
    | Prominent
    /// Always visible.
    | Omnipresent 
    member this.EnclosedType with get () = match this with Enclosed -> true | _ -> false
    member this.ExposedType with get () = match this with Exposed -> true | _ -> false
    member this.ImposterType with get () = match this with Imposter -> true | _ -> false
    member this.ProminentType with get () = match this with Prominent -> true | _ -> false
    member this.OmnipresentType with get () = match this with Omnipresent -> true | _ -> false

[<AutoOpen>]
module PresenceOperators =
    
    /// Test two presence values for equality without allocating.
    let presenceEq left right =
        match (left, right) with
        | (Enclosed, Enclosed)
        | (Exposed, Exposed)
        | (Imposter, Imposter)
        | (Prominent, Prominent)
        | (Omnipresent, Omnipresent) -> true
        | (_, _) -> false

    /// Test two presence values for inequality.
    let presenceNeq left right =
        not (presenceEq left right)