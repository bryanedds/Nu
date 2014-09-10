// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Nu
open System
open System.Reflection
open System.ComponentModel
open Microsoft.FSharp.Reflection
open Prime
open Nu.NuConstants

[<AutoOpen>]
module XtensionModule =

    /// The empty dispatcher.
    /// NOTE: this could instead be a special class with a MethodMissing method
    let private EmptyDispatcher = new obj ()

    /// An attribute to specify the default value of an XField.
    [<AttributeUsage (AttributeTargets.Class)>]
    type XDefaultValueAttribute (defaultValue : obj) =
        inherit Attribute ()
        member this.DefaultValue = defaultValue
        
    /// Describes an XField.
    type XFieldDescriptor =
        { FieldName : string
          TypeName : string } // the .NET type name

    /// An indexible collection of XFields.
    type XFields = Map<string, obj>

    /// Xtensions (and their supporting types) are a dynamic, functional, and semi-convenient way
    /// to solve the 'expression problem' in F#, and can also be used to implement a dynamic
    /// 'Entity-Component System'.
    type [<StructuralEquality; NoComparison>] Xtension =
        { XFields : XFields
          OptXDispatcherName : string option
          CanDefault : bool
          Sealed : bool }

        /// Get the default value of an instance of type 'r taking into account XDefaultValue decorations.
        static member private getDefaultValue () : 'r =
            let defaultFieldType = typeof<'r>
            let optDefaultValueAttribute = Seq.tryHead <| defaultFieldType.GetCustomAttributes<XDefaultValueAttribute> ()
            match optDefaultValueAttribute with
            | Some defaultValueAttribute ->
                match defaultValueAttribute.DefaultValue with
                | :? 'r as defaultValue -> defaultValue
                | _ as defaultValue ->
                    let converter = TypeDescriptor.GetConverter defaultFieldType
                    let defaultValueType = defaultValue.GetType ()
                    if converter.CanConvertFrom defaultValueType then converter.ConvertFrom defaultValue :?> 'r
                    else failwith <| "Cannot convert '" + string defaultValue + "' to type '" + defaultFieldType.Name + "'."
            | None -> Unchecked.defaultof<'r>

        /// Try to get the default value for a given xtension member, returning None when defaulting is disallowed.
        static member private tryGetDefaultValue (this : Xtension) memberName : 'r =
            if this.CanDefault then Xtension.getDefaultValue ()
            else failwith <| "The Xtension field '" + memberName + "' does not exist and no default is permitted because CanDefault is false."

        /// Get a dispatcher by its name from the given dispatcher container.
        static member getDispatcherByName dispatcherName (dispatcherContainer : IXDispatcherContainer) =
            let dispatchers = dispatcherContainer.GetDispatchers ()
            match Map.tryFind dispatcherName dispatchers with
            | Some dispatcher -> dispatcher
            | None -> failwith <| "Invalid XDispatcher '" + dispatcherName + "'."
        
        /// Get the assigned dispatcher for an xtension.
        static member getDispatcher xtension (dispatcherContainer : IXDispatcherContainer) =
            let optXDispatcherName = xtension.OptXDispatcherName
            match optXDispatcherName with
            | Some dispatcherName -> Xtension.getDispatcherByName dispatcherName dispatcherContainer
            | None -> EmptyDispatcher

        /// Queries that the target type offers dispatcher behavior congruent to the given dispatcher.
        static member dispatchesAs2 (dispatcherTargetType : Type) dispatcher =
            let dispatcherType = dispatcher.GetType ()
            let result =
                dispatcherTargetType = dispatcherType ||
                dispatcherType.IsSubclassOf dispatcherTargetType
            result

        /// Queries that the target type offers dispatcher behavior congruent to the given xtension.
        static member dispatchesAs (dispatcherTargetType : Type) xtension dispatcherContainer =
            let dispatcher = Xtension.getDispatcher xtension dispatcherContainer
            Xtension.dispatchesAs2 dispatcherTargetType dispatcher

        /// The dynamic dispatch operator.
        /// Example -   let parallax = entity?Parallax () : single
        /// Example2 -  let renderDescriptor = entity?GetRenderDescriptor () : RenderDescriptor
        /// TODO: search for a way to effectively optimize this function, especially in regard
        /// to its use of MethodInfo.Invoke.
        static member (?) (xtension, memberName) : 'a -> 'r =

            // NOTE: I think the explicit args abstraction is required here to satisfy the signature
            // for op_Dynamic... maybe.
            fun args ->

                // check if dynamic member is an existing field
                match Map.tryFind memberName xtension.XFields with

                | Some field ->
                    
                    // return field directly if the return type matches, otherwise the default value for that type
                    match field with
                    | :? 'r as fieldValue -> fieldValue
                    | _ -> Xtension.tryGetDefaultValue xtension memberName

                | None ->

                    // try to convert method args to an array
                    let optArgArray =
                        if box args = null then null // not sure what this line is meant to do
                        elif FSharpType.IsTuple <| args.GetType () then FSharpValue.GetTupleFields args
                        else [|args|]

                    // check if arg array exists - this will tell us if the user is attempting to access a field or a
                    // dispatch
                    match optArgArray with
                    | null ->

                        // presume we're looking for a field that doesn't exist, so try to get the default value
                        Xtension.tryGetDefaultValue xtension memberName

                    | argArray ->

                        // presume we're looking for a dispatch, so ensure the last arg is a dispatcher container
                        // NOTE: Array.last is a linear-time operation.
                        match Array.last argArray with
                        | :? IXDispatcherContainer as context ->

                            // attempt to dispatch method
                            let dispatcher = Xtension.getDispatcher xtension context
                            let dispatcherType = dispatcher.GetType ()
                            match dispatcherType.GetMethod memberName with
                            | null -> failwith <| "Could not find method '" + memberName + "' on XDispatcher '" + dispatcherType.Name + "'."
                            | aMethod ->
                                try aMethod.Invoke (dispatcher, argArray) :?> 'r with
                                | exn when exn.InnerException <> null -> raise exn.InnerException
                                | exn ->
                                    debug <| "Unknown failure during XDispatch invocation'" + string exn + "'."
                                    reraise ()

                        | _ -> failwith "Last argument of Xtension method call must be an IXDispatcherContainer or sub-type."

        /// The dynamic assignment operator for an Xtension.
        /// Example - let entity = entity.Position <- Vector2 (4.0, 5.0).
        static member (?<-) (xtension, fieldName, value) =
    #if DEBUG
            // nop'ed outside of debug mode for efficiency
            // TODO: consider writing a 'Map.addDidContainKey' function to efficently add and return a
            // result that the key was already contained.
            if xtension.Sealed && not <| Map.containsKey fieldName xtension.XFields
            then failwith "Cannot add field to a sealed Xtension."
            else
    #endif
            let xFields = Map.add fieldName (value :> obj) xtension.XFields
            { xtension with XFields = xFields }

    /// A collection of objects that can handle dynamically dispatched messages via reflection.
    /// These are just POFSO types, except without any data (the data they use would be in a related
    /// value's XField).
    and XDispatchers =
        Map<string, obj>

    /// Represents a container of XDispatchers.
    and IXDispatcherContainer =
        interface
            abstract GetDispatchers : unit -> XDispatchers
            end

[<RequireQualifiedAccess>]
module Xtension =

    /// An Xtension that can default and isn't sealed.
    let empty = { XFields = Map.empty; OptXDispatcherName = None; CanDefault = true; Sealed = false }

    /// An Xtension that cannot default and is sealed.
    let safe = { XFields = Map.empty; OptXDispatcherName = None; CanDefault = false; Sealed = true }

    /// An Xtension that cannot default and isn't sealed.
    let mixed = { XFields = Map.empty; OptXDispatcherName = None; CanDefault = false; Sealed = false }

    /// Make an extension with custom safety.
    let make canDefault isSealed =
        { XFields = Map.empty; OptXDispatcherName = None; CanDefault = canDefault; Sealed = isSealed }