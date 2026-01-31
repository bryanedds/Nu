// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<RequireQualifiedAccess>]
module Xtension =

    /// Provides a convenient way to implement both dynamic properties and designer properties.
    type [<ReferenceEquality>] Xtension =
        private
            { Properties_ : Dictionary<string, Property> // TODO: see if a quadratic searching dictionary could improve perf here.
              mutable ContainsRuntimeProperties_ : bool }

        member this.Properties = this.Properties_
        member this.ContainsRuntimeProperties = this.ContainsRuntimeProperties_

    /// Get the properties of an Xtension.
    let getProperties (xtension : Xtension) =
        xtension.Properties_

    /// Check whether the Xtension contains any DesignerProperty's or ComputedProperty's in constant-time (via an
    /// internally-cached flag).
    let containsRuntimeProperties (xtension : Xtension) =
        xtension.ContainsRuntimeProperties_

    /// Try to get a property from an Xtension.
    let tryGetProperty (name, xtension, propertyRef : _ byref) =
        xtension.Properties_.TryGetValue (name, &propertyRef)

    /// Get a property from an xtension.
    let getProperty name xtension =
        xtension.Properties_.[name]

    /// Attempt to set a property on an Xtension.
    let trySetProperty name property xtension =
        let mutable propertyRef = Unchecked.defaultof<_>
        if xtension.Properties_.TryGetValue (name, &propertyRef) then
            propertyRef.PropertyValue <- property.PropertyValue
            true
        else false

    /// Set a property on an Xtension.
    let setProperty name property xtension =
        if not (trySetProperty name property xtension) then
            failwith "Cannot set property to an Xtension without first attaching it."

    /// Attach a property to an Xtension.
    let attachProperty name property xtension =
        if Reflection.isRuntimeProperty property then xtension.ContainsRuntimeProperties_ <- true
        xtension.Properties_.[name] <- property

    /// Attach multiple properties to an Xtension.
    let attachProperties properties xtension =
        if Reflection.containsRuntimeProperties properties then xtension.ContainsRuntimeProperties_ <- true
        for (name, property) in properties do
            xtension.Properties_.[name] <- property

    /// Detach a property from an Xtension.
    let detachProperty name xtension =
        xtension.Properties_.Remove name |> ignore<bool>
        xtension.ContainsRuntimeProperties_ <- Reflection.containsRuntimeProperties xtension.Properties_.Pairs

    /// Detach multiple properties from an Xtension.
    let detachProperties names xtension =
        for name in names do
            xtension.Properties_.Remove name |> ignore<bool>
        xtension.ContainsRuntimeProperties_ <- Reflection.containsRuntimeProperties xtension.Properties_.Pairs

    /// Make an Xtension.
    let make properties =
        { Properties_ = properties
          ContainsRuntimeProperties_ = Reflection.containsRuntimeProperties properties.Pairs }

    /// Make an empty Xtension.
    let makeEmpty () =
        make (dictPlus StringComparer.Ordinal [])

    /// Convert an xtension to a sequence of its entries.
    let makeFromProperties seq =
        attachProperties seq (makeEmpty ())

    /// Make an Xtension by copying properties of another Xtension.
    let makeFromXtension (xtension : Xtension) = 
        xtension.Properties_.Pairs
        |> Seq.map (fun (n, p) ->
            let propertyValue =
                match p.PropertyValue with
                | :? DesignerProperty as dp -> { dp with DesignerType = dp.DesignerType } :> obj
                | value -> value
            (n, { p with PropertyValue = propertyValue }))
        |> makeFromProperties

/// Provides a convenient way to implement both dynamic properties and designer properties.
type Xtension = Xtension.Xtension