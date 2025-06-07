// Nu Game Engine.
// Copyright (C) Bryan Edds.

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
            { Properties : Dictionary<string, Property> // TODO: see if a quadratic searching dictionary could improve perf here.
              mutable ContainsRuntimeProperties : bool }

        /// The dynamic look-up operator for an Xtension.
        /// Example:
        ///     let parallax : single = xtn?Parallax
        static member (?) (xtension, propertyName) : 'a =

            // try to find an existing property
            match xtension.Properties.TryGetValue propertyName with
            | (true, property) ->

                // return property directly if the return type matches, otherwise the default value for that type
                match property.PropertyValue with
                | :? DesignerProperty as dp ->
                    match dp.DesignerValue with
                    | :? 'a as propertyValue -> propertyValue
                    | _ -> failwith ("Xtension property '" + propertyName + "' of type '" + property.PropertyType.Name + "' is not of the expected type '" + typeof<'a>.Name + "'.")
                | :? 'a as value -> value
                | _ -> failwith ("Xtension property '" + propertyName + "' of type '" + property.PropertyType.Name + "' is not of the expected type '" + typeof<'a>.Name + "'.")

            // can't find the required property.
            | (false, _) -> failwith ("No property '" + propertyName + "' found.")

        /// The dynamic assignment operator for an Xtension.
        /// Example:
        ///     let xtn = xtn.Position <- Vector2 (4.0, 5.0).
        static member (?<-) (xtension, propertyName, value : 'a) =
            match xtension.Properties.TryGetValue propertyName with
            | (true, property) ->
#if DEBUG
                if property.PropertyType <> typeof<'a> then
                    failwith "Cannot change the type of an existing Xtension property."
#endif
                match property.PropertyValue with
                | :? DesignerProperty as dp -> dp.DesignerValue <- value :> obj
                | _ -> property.PropertyValue <- value :> obj
            | (false, _) -> failwith "Cannot set property to an Xtension without first attaching it."

    /// Check whether the Xtension contains any DesignerProperty's or ComputedProperty's in constant-time (via an
    /// internally-cached flag).
    let containsRuntimeProperties (xtension : Xtension) = xtension.ContainsRuntimeProperties

    /// Try to get a property from an Xtension.
    let tryGetProperty (name, xtension, propertyRef : _ byref) = xtension.Properties.TryGetValue (name, &propertyRef)

    /// Get a property from an xtension.
    let getProperty name xtension = xtension.Properties.[name]

    /// Attempt to set a property on an Xtension.
    let trySetProperty name property xtension =
        let mutable propertyRef = Unchecked.defaultof<_>
        if xtension.Properties.TryGetValue (name, &propertyRef) then
#if DEBUG
            if property.PropertyType <> propertyRef.PropertyType then
                failwith "Cannot change the type of an existing Xtension property."
#endif
            propertyRef.PropertyValue <- property.PropertyValue
            true
        else false

    /// Set a property on an Xtension.
    let setProperty name property xtension =
        if not (trySetProperty name property xtension) then
            failwith "Cannot set property to an Xtension without first attaching it."

    /// Attach a property to an Xtension.
    let attachProperty name property xtension =
        if Reflection.isRuntimeProperty property then xtension.ContainsRuntimeProperties <- true
        xtension.Properties.[name] <- property

    /// Attach multiple properties to an Xtension.
    let attachProperties properties xtension =
        if Reflection.containsRuntimeProperties properties then xtension.ContainsRuntimeProperties <- true
        for (name, property) in properties do
            xtension.Properties.[name] <- property

    /// Detach a property from an Xtension.
    let detachProperty name xtension =
        xtension.Properties.Remove name |> ignore<bool>
        xtension.ContainsRuntimeProperties <- Reflection.containsRuntimeProperties xtension.Properties.Pairs

    /// Detach multiple properties from an Xtension.
    let detachProperties names xtension =
        for name in names do
            xtension.Properties.Remove name |> ignore<bool>
        xtension.ContainsRuntimeProperties <- Reflection.containsRuntimeProperties xtension.Properties.Pairs

    /// Make an Xtension.
    let make properties =
        { Properties = properties
          ContainsRuntimeProperties = Reflection.containsRuntimeProperties properties.Pairs }

    /// Make an empty Xtension.
    let makeEmpty () =
        make (dictPlus StringComparer.Ordinal [])

    /// Make an Xtension by copying properties of another Xtension.
    let rec makeFromXtension (xtension : Xtension) = 
        xtension |>
        toSeq |>
        Seq.map (fun (n, p) ->
            let propertyValue =
                match p.PropertyValue with
                | :? DesignerProperty as dp -> { dp with DesignerType = dp.DesignerType } :> obj
                | value -> value
            (n, { p with PropertyValue = propertyValue })) |>
        ofSeq

    /// Convert an xtension to a sequence of its entries.
    and toSeq (xtension : Xtension) =
        xtension.Properties.Pairs :> _ seq

    /// Convert an xtension to a sequence of its entries.
    and ofSeq seq =
        attachProperties seq (makeEmpty ())

/// Provides a convenient way to implement both dynamic properties and designer properties.
type Xtension = Xtension.Xtension