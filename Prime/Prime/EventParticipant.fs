// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open Prime

/// A participant in the event system.
type Participant =
    interface
        abstract member ParticipantAddress : Participant Address
        end

/// Operators for the Participant type.
type ParticipantOperators =
    private
        | ParticipantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (participant : Participant) = acatf address (atooa participant.ParticipantAddress)

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, participant : Participant) = ParticipantOperators.acatf address participant

/// The data for a change in a participant.
type [<StructuralEquality; NoComparison>] ParticipantChangeData<'p, 'w when 'p :> Participant> =
    { Participant : 'p
      PropertyName : string
      OldWorld : 'w }

/// Describes a property of a participant.
/// Similar to a Haskell lens, but specialized to properties.
type [<NoEquality; NoComparison>] PropertyTag<'s, 'a, 'w when 's :> Participant> =
    { This : 's
      Name : string
      Get : 'w -> 'a
      SetOpt : ('a -> 'w -> 'w) option }

    member this.Map mapper : PropertyTag<'s, 'a, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (mapper value)) | None -> None }

    member this.Map2 mapper unmapper : PropertyTag<'s, 'b, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (unmapper value)) | None -> None }

    member this.MapOut mapper : PropertyTag<'s, 'b, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = None }

    member this.Change =
        let changeEventAddress = Address<ParticipantChangeData<'s, 'w>>.ltoa [typeof<'s>.Name; "Change"; this.Name; "Event"]
        let changeEvent = changeEventAddress ->>- this.This.ParticipantAddress
        changeEvent

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PropertyTag =

    let map mapper (property : PropertyTag<_, _, _>) =
        property.Map mapper

    let map2 mapper unmapper (property : PropertyTag<_, _, _>) =
        property.Map2 mapper unmapper

    let mapOut mapper (property : PropertyTag<_, _, _>) =
        property.MapOut mapper

    let makeReadOnly this name get =
        { This = this; Name = name; Get = get; SetOpt = None }

    let make this name get set =
        { This = this; Name = name; Get = get; SetOpt = Some set }