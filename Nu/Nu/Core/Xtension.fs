// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Xtension =

    let [<Literal>] private ImperativeMask =                0b0001
    let [<Literal>] private ContainsRuntimePropertiesMask = 0b0010

    /// Provides a convenient way to implement both dynamic properties and designer properties.
    type [<ReferenceEquality>] Xtension =
        private
            { Properties_ : UMap<string, Property> // TODO: see if a quadratic searching dictionary could improve perf here.
              Flags_ : int }

        member this.Properties = this.Properties_
        member this.Imperative = this.Flags_ &&& ImperativeMask <> 0
        member this.ContainsRuntimeProperties = this.Flags_ &&& ContainsRuntimePropertiesMask <> 0

    /// Get the properties of an Xtension.
    let getProperties (xtension : Xtension) =
        xtension.Properties_

    /// Check whether the Xtension uses mutation.
    let getImperative (xtension : Xtension) =
        xtension.Imperative

    /// Check whether the Xtension contains any DesignerProperty's or ComputedProperty's in constant-time (via an
    /// internally-cached flag).
    let containsRuntimeProperties (xtension : Xtension) =
        xtension.ContainsRuntimeProperties

    /// Try to get a property from an Xtension.
    let tryGetProperty (name, xtension, propertyRef : _ outref) =
        UMap.tryGetValue (name, xtension.Properties_, &propertyRef)

    /// Get a property from an xtension.
    let getProperty name xtension = UMap.find name xtension.Properties_

    /// Attempt to set a property on an Xtension.
    let trySetProperty name property xtension =
        let mutable propertyRef = Unchecked.defaultof<_>
        match UMap.tryGetValue (name, xtension.Properties_, &propertyRef) with
        | true ->
            if xtension.Imperative then
                propertyRef.PropertyValue <- property.PropertyValue
                struct (true, xtension)
            else struct (true, { xtension with Properties_ = UMap.add name property xtension.Properties_ })
        | false -> struct (false, xtension)

    /// Set a property on an Xtension.
    let setProperty name property xtension =
        match trySetProperty name property xtension with
        | struct (true, xtension) -> xtension
        | struct (false, _) -> failwith "Cannot set property to an Xtension without first attaching it."

    /// Attach a property to an Xtension.
    let attachProperty name property xtension =
        let isRuntimeProperty = if Reflection.isRuntimeProperty property then ContainsRuntimePropertiesMask else 0
        { xtension with Properties_ = UMap.add name property xtension.Properties_; Flags_ = xtension.Flags_ ||| isRuntimeProperty }

    /// Attach multiple properties to an Xtension.
    let attachProperties properties xtension =
        let containsRuntimeProperties = if Reflection.containsRuntimeProperties properties then ContainsRuntimePropertiesMask else 0
        { xtension with Properties_ = UMap.addMany properties xtension.Properties_; Flags_ = xtension.Flags_ ||| containsRuntimeProperties }

    /// Detach a property from an Xtension.
    let detachProperty name xtension =
        let properties = UMap.remove name xtension.Properties_
        let containsRuntimeProperties = if Reflection.containsRuntimeProperties xtension.Properties_ then ContainsRuntimePropertiesMask else 0
        { xtension with  
            Properties_ = properties
            Flags_ = xtension.Flags_ &&& ImperativeMask ||| containsRuntimeProperties }

    /// Detach multiple properties from an Xtension.
    let detachProperties names xtension =
        let properties = UMap.removeMany names xtension.Properties_
        let containsRuntimeProperties = if Reflection.containsRuntimeProperties xtension.Properties_ then ContainsRuntimePropertiesMask else 0
        { xtension with
            Properties_ = properties
            Flags_ = xtension.Flags_ &&& ImperativeMask ||| containsRuntimeProperties }

    /// Make an Xtension.
    let make imperative properties =
        { Properties_ = properties
          Flags_ =
            (if imperative then ImperativeMask else 0) |||
            (if Reflection.containsRuntimeProperties properties then ContainsRuntimePropertiesMask else 0) }

    /// Make an empty Xtension.
    let makeEmpty imperative =
        make imperative (UMap.makeEmpty StringComparer.Ordinal (if imperative then Imperative else Functional))

    /// An Xtension that is imperative.
    let makeImperative () =
        make true (UMap.makeEmpty StringComparer.Ordinal Imperative)

    /// An Xtension that isn't imperative.
    let makeFunctional () =
        make false (UMap.makeEmpty StringComparer.Ordinal Functional)

    /// Convert an xtension to a sequence of its entries.
    let makeFromProperties imperative seq =
        attachProperties seq (makeEmpty imperative)

    /// Make an Xtension by copying properties of another Xtension.
    let makeFromXtension (xtension : Xtension) = 
        xtension.Properties_
        |> Seq.map (fun (n, p) ->
            let propertyValue =
                match p.PropertyValue with
                | :? DesignerProperty as dp -> { dp with DesignerType = dp.DesignerType } :> obj
                | value -> value
            (n, { p with PropertyValue = propertyValue }))
        |> makeFromProperties xtension.Imperative

/// Provides a convenient way to implement both dynamic properties and designer properties.
type Xtension = Xtension.Xtension