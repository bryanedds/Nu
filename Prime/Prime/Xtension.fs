// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open Prime

[<AutoOpen>]
module XtensionModule =

    /// Xtensions are a dynamic, functional, and convenient way to implement both dynamic properties
    /// and designer properties.
    type [<NoEquality; NoComparison>] Xtension =
        private
            { Properties : PropertyMap
              CanDefault : bool
              Sealed : bool
              Imperative : bool }

        /// Try to get the default value for a given xtension member, returning None when defaulting is disallowed.
        static member private tryGetDefaultValue (this : Xtension) propertyName : 'a =
            if this.CanDefault then scdefaultof ()
            else failwith ("Xtension property '" + propertyName + "' does not exist and no default is permitted because CanDefault is false.")

        /// The dynamic look-up operator for an Xtension.
        /// Example:
        ///     let parallax = xtn?Parallax : single
        static member (?) (xtension, propertyName) : 'a =

            // check if dynamic member is an existing property
            let propertyOpt = UMap.tryFindFast propertyName xtension.Properties
            if FOption.isSome propertyOpt then
                
                // return property directly if the return type matches, otherwise the default value for that type
                let property = FOption.get propertyOpt
                match property.PropertyValue with
                | :? DesignerProperty as dp ->
                    match dp.DesignerValue with
                    | :? 'a as propertyValue -> propertyValue
                    | _ -> failwith ("Xtension property '" + propertyName + "' of type '" + property.PropertyType.Name + "' is not of the expected type '" + typeof<'a>.Name + "'.")
                | :? 'a as value -> value
                | _ -> failwith ("Xtension property '" + propertyName + "' of type '" + property.PropertyType.Name + "' is not of the expected type '" + typeof<'a>.Name + "'.")

            else

                // presume we're looking for a property that doesn't exist, so try to get the default value
                Xtension.tryGetDefaultValue xtension propertyName

        /// The dynamic assignment operator for an Xtension.
        /// Example:
        ///     let xtn = xtn.Position <- Vector2 (4.0, 5.0).
        static member (?<-) (xtension, propertyName, value : 'a) =
            if typeof<'a> = typeof<DesignerProperty> then failwith "Cannot directly set an Xtension property to a DesignerProperty."
            let propertyOpt = UMap.tryFindFast propertyName xtension.Properties
            if FOption.isSome propertyOpt then
                let mutable property = FOption.get propertyOpt
                if xtension.Sealed && property.PropertyType <> typeof<'a> then failwith "Cannot change the type of a sealed Xtension's property."
                if xtension.Imperative then
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerValue <- value :> obj
                    | _ -> property.PropertyValue <- value :> obj
                    xtension
                else
                    match property.PropertyValue with
                    | :? DesignerProperty as dp ->
                        let property = { property with PropertyValue = { dp with DesignerValue = value }}
                        let properties = UMap.add propertyName property xtension.Properties
                        { xtension with Properties = properties }
                    | _ ->
                        let property = { property with PropertyValue = value :> obj }
                        let properties = UMap.add propertyName property xtension.Properties
                        { xtension with Properties = properties }
            else
                if xtension.Sealed then failwith "Cannot add property to a sealed Xtension."
                let property = { PropertyType = typeof<'a>; PropertyValue = value :> obj }
                let properties = UMap.add propertyName property xtension.Properties
                { xtension with Properties = properties }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Xtension =
    
        /// The TConfig of Xtension's T/U structures.
        let Config = Functional
    
        /// Make an extension.
        let make properties canDefault isSealed imperative =
            { Properties = properties
              CanDefault = canDefault
              Sealed = isSealed
              Imperative = imperative }
    
        /// An Xtension that cannot default, is sealed, and is imperative.
        let makeImperative () = make (UMap.makeEmpty Config) false true true
    
        /// An Xtension that can default, isn't sealed, and isn't imperative.
        let makeEmpty () = make (UMap.makeEmpty Config) true false false
    
        /// An Xtension that cannot default, is sealed, and isn't imperative.
        let makeSafe () = make (UMap.makeEmpty Config) false true false
    
        /// An Xtension that cannot default, isn't sealed, and isn't imperative.
        let makeMixed () = make (UMap.makeEmpty Config) false false false

        /// Whether the extension uses mutation.
        let getImperative xtension = xtension.Imperative

        /// Try to get a property from an xtension.
        let tryGetProperty name xtension = UMap.tryFind name xtension.Properties

        /// Get a property from an xtension.
        let getProperty name xtension = UMap.find name xtension.Properties

        /// Set a property on an Xtension.
        let trySetProperty name property xtension =
            match UMap.tryFind name xtension.Properties with
            | Some property' ->
                if xtension.Imperative then
                    let mutable property' = property' // rebind as mutable
                    property'.PropertyType <- property.PropertyType
                    property'.PropertyValue <- property.PropertyValue
                    (true, xtension)
                else (true, { xtension with Properties = UMap.add name property xtension.Properties })
            | None ->
                if not xtension.Sealed
                then (true, { xtension with Properties = UMap.add name property xtension.Properties })
                else (false, xtension)

        /// Set a property on an Xtension.
        let setProperty name property xtension =
            match trySetProperty name property xtension with
            | (true, xtension) -> xtension
            | (false, _) -> failwith "Cannot add property to a sealed Xtension."

        /// Attach a property to an Xtension.
        let attachProperty name property xtension = { xtension with Properties = UMap.add name property xtension.Properties }
    
        /// Attach multiple properties to an Xtension.
        let attachProperties namesAndProperties xtension = { xtension with Properties = UMap.addMany namesAndProperties xtension.Properties }
    
        /// Detach a property from an Xtension.
        let detachProperty name xtension = { xtension with Properties = UMap.remove name xtension.Properties }
    
        /// Detach multiple properties from an Xtension.
        let detachProperties names xtension = { xtension with Properties = UMap.removeMany names xtension.Properties }
    
        /// Convert an xtension to a sequence of its entries.
        let toSeq xtension = xtension.Properties :> _ seq

        /// Convert an xtension to a sequence of its entries.
        let ofSeq seq = attachProperties seq (makeEmpty ())

/// Xtensions (and their supporting types) are a dynamic, functional, and convenient way
/// to implement dynamic properties.
type Xtension = XtensionModule.Xtension
