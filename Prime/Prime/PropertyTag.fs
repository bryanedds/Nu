// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open Prime

/// Describes a property.
/// Similar to a Haskell lens, but specialized to properties.
type [<NoEquality; NoComparison>] PropertyTag<'s, 'a, 'w> =
    { This : 's
      Name : string
      Get : 'w -> 'a
      OptSet : ('a -> 'w -> 'w) option }

    member this.Map mapper : PropertyTag<'s, 'a, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          OptSet = match this.OptSet with Some set -> Some (fun value -> set (mapper value)) | None -> None }

    member this.Map2 mapper unmapper : PropertyTag<'s, 'b, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          OptSet = match this.OptSet with Some set -> Some (fun value -> set (unmapper value)) | None -> None }

    member this.MapOut mapper : PropertyTag<'s, 'b, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          OptSet = None }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PropertyTag =

    let map mapper (property : PropertyTag<_, _, _>) =
        property.Map mapper

    let map2 mapper unmapper (property : PropertyTag<_, _, _>) =
        property.Map2 mapper unmapper

    let mapOut mapper (property : PropertyTag<_, _, _>) =
        property.MapOut mapper

    let makeReadOnly this name get =
        { This = this; Name = name; Get = get; OptSet = None }

    let make this name get set =
        { This = this; Name = name; Get = get; OptSet = Some set }