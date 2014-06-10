// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open System.Reflection
open System.ComponentModel
open System.Xml
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module XtensionModule =

    /// An attribute to specify the default value of an XField.
    [<AttributeUsage (AttributeTargets.Class)>]
    type XDefaultValueAttribute (defaultValue : obj) =
        inherit Attribute ()
        member this.DefaultValue = defaultValue
        
    /// An attribute to specify a property extension as an XField.
    [<AttributeUsage (AttributeTargets.Property)>]
    type XFieldAttribute () =
        inherit Attribute ()

    /// Describes an XField.
    type XFieldDescriptor =
        { FieldName : string
          TypeName : string } // the .NET type name

    /// An indexible collection of XFields.
    type XFields =
        Map<string, obj>

    /// Xtensions (and their supporting types) are a dynamic, functional, and semi-convenient way
    /// to solve the 'expression problem' in F#, and can also be used to implement a dynamic
    /// 'Entity-Component System'.
    type [<StructuralEqualityAttribute; NoComparison>] Xtension =
        { XFields : XFields
          OptXDispatcherName : string option
          CanDefault : bool
          Sealed : bool }

        // NOTE: this could instead be a special class with a MethodMissing method
        static member private emptyDispatcher = new obj ()

        static member private getDefaultValue (this : Xtension) : 'r =
            let defaultFieldType = typeof<'r>
            let optDefaultValueAttribute = Seq.tryHead <| defaultFieldType.GetCustomAttributes<XDefaultValueAttribute> ()
            match optDefaultValueAttribute with
            | None -> Unchecked.defaultof<'r>
            | Some defaultValueAttribute ->
                match defaultValueAttribute.DefaultValue with
                | :? 'r as defaultValue -> defaultValue
                | _ as defaultValue ->
                    let converter = TypeDescriptor.GetConverter defaultFieldType
                    let defaultValueType = defaultValue.GetType ()
                    if not <| converter.CanConvertFrom defaultValueType
                    then failwith <| "Cannot convert '" + string defaultValue + "' to type '" + defaultFieldType.Name + "'."
                    else converter.ConvertFrom defaultValue :?> 'r

        static member private tryGetDefaultValue (this : Xtension) memberName : 'r =
            if not this.CanDefault
            then failwith <| "The Xtension field '" + memberName + "' does not exist and no default is permitted because CanDefault is false."
            else Xtension.getDefaultValue this

        static member getDispatcherByName dispatcherName (dispatcherContainer : IXDispatcherContainer) =
                let dispatchers = dispatcherContainer.GetDispatchers ()
                match Map.tryFind dispatcherName dispatchers with
                | None -> failwith <| "Invalid XDispatcher '" + dispatcherName + "'."
                | Some dispatcher -> dispatcher

        static member getDispatcher xtension (dispatcherContainer : IXDispatcherContainer) =
            let optXDispatcherName = xtension.OptXDispatcherName
            match optXDispatcherName with
            | None -> Xtension.emptyDispatcher
            | Some dispatcherName -> Xtension.getDispatcherByName dispatcherName dispatcherContainer

        static member derivesFrom dispatcherTarget xtension dispatcherContainer =
            let dispatcher = Xtension.getDispatcher xtension dispatcherContainer
            let dispatcherType = dispatcherTarget.GetType ()
            let dispatcherTargetType = dispatcherTarget.GetType ()
            dispatcherType.IsSubclassOf dispatcherTargetType

        static member derivesFromByName dispatcherTargetName xtension dispatcherContainer =
            let dispatcher = Xtension.getDispatcher xtension dispatcherContainer
            let dispatcherTarget = Xtension.getDispatcherByName dispatcherTargetName dispatcherContainer
            let dispatcherType = dispatcher.GetType ()
            let dispatcherTargetType = dispatcherTarget.GetType ()
            dispatcherType.IsSubclassOf dispatcherTargetType

        /// The dynamic dispatch operator.
        static member (?) (target, xtension, memberName) : 'a -> 'r =

            // NOTE: I think the explicit args abstraction is required here to satisfy the signature
            // for op_Dynamic... maybe.
            fun args ->

                // check if dynamic member is an existing field
                match Map.tryFind memberName xtension.XFields with
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

                            // include 'target' context as first arg
                            // NOTE: array appending is a linear-time operation, but is currently required to satisfy
                            // the MethodInfo.Invoke interface.
                            let args = Array.append [|target :> obj|] argArray

                            // find dispatcher, or use the empty dispatcher
                            

                            // attempt to dispatch method
                            let dispatcher = Xtension.getDispatcher xtension context
                            let dispatcherType = dispatcher.GetType ()
                            match dispatcherType.GetMethod memberName with
                            | null -> failwith <| "Could not find method '" + memberName + "' on XDispatcher '" + dispatcherType.Name + "'."
                            | aMethod ->
                                try aMethod.Invoke (dispatcher, args) :?> 'r with
                                | exn when exn.InnerException <> null -> raise exn.InnerException
                                | exn ->
                                    debug <| "Unknown failure during XDispatch invocation'" + string exn + "'."
                                    reraise ()

                        | _ -> failwith "Last argument of Xtension method call must be an IXDispatcherContainer or sub-type."

                // return field directly if the return type matches, otherwise the default value for that type
                | Some field ->
                    match field with
                    | :? 'r as fieldValue -> fieldValue
                    | _ -> Xtension.tryGetDefaultValue xtension memberName

        /// The dynamic dispatch operator for use on raw Xtension values.
        static member (?) (xtension, memberName) : 'a -> 'r =
            Xtension.(?) (xtension, xtension, memberName)

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
    /// These are just POFO types, except without any data (the data they use would be in a related
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

    /// The empty Xtension.
    let empty = { XFields = Map.empty; OptXDispatcherName = None; CanDefault = true; Sealed = false }

    /// Is a property with the give name writable?
    let isPropertyNameWriteable (propertyName : string) =
        not <| propertyName.EndsWith "Id" && // don't write an Id
        not <| propertyName.EndsWith "Ids" && // don't write multiple Ids
        not <| propertyName.EndsWith "Ns" // 'Ns' stands for 'Not serializable'.

    /// Is the given property writable?
    let isPropertyWriteable (property : PropertyInfo) =
        property.CanWrite &&
        isPropertyNameWriteable property.Name

    /// Read an Xtension from Xml.
    let read (valueNode : XmlNode) =
        let optXDispatcherName = match valueNode.Attributes.["xDispatcher"].InnerText with "" -> None | str -> Some str
        let childNodes = enumerable valueNode.ChildNodes
        let xFields =
            Seq.map
                (fun (xNode : XmlNode) ->
                    let typeName = xNode.Attributes.["type"].InnerText
                    let aType = findType typeName
                    let xValueStr = xNode.InnerText
                    let converter = TypeDescriptor.GetConverter aType
                    if not <| converter.CanConvertFrom typeof<string>
                    then failwith <| "Cannot convert string '" + xValueStr + "' to type '" + typeName + "'."
                    else (xNode.Name, converter.ConvertFrom xValueStr))
                childNodes
        { XFields = Map.ofSeq xFields; OptXDispatcherName = optXDispatcherName; CanDefault = true; Sealed = false }

    /// Attempt to read a property from Xml.
    let tryReadProperty (property : PropertyInfo) (valueNode : XmlNode) (target : 'a) =
        if property.PropertyType = typeof<Xtension> then
            let xtension = read valueNode
            property.SetValue (target, xtension)
        else
            let valueStr = valueNode.InnerText
            let converter = TypeDescriptor.GetConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFrom valueStr
                property.SetValue (target, value)

    /// Read a property from Xml if possible.
    let readProperty<'a> (fieldNode : XmlNode) (target : 'a) =
        match typeof<'a>.GetPropertyWritable (fieldNode.Name, BindingFlags.Public ||| BindingFlags.Instance) with
        | None -> ()
        | Some property -> tryReadProperty property fieldNode target

    /// Read all properties from Xml.
    let readProperties<'a> (modelNode : XmlNode) (target : 'a) =
        for node in modelNode.ChildNodes do
            readProperty<'a> node target

    /// Write an Xtension to Xml.
    /// TODO: need a vanilla write function that writes to an XmlDocument rather than directly to an XmlWriter stream.
    let writeToXmlWriter (writer : XmlWriter) xtension =
        writer.WriteAttributeString ("xDispatcher", match xtension.OptXDispatcherName with None -> String.Empty | Some name -> name)
        for xField in xtension.XFields do
            let xFieldName = xField.Key
            if isPropertyNameWriteable xFieldName then
                let xValue = xField.Value
                let xDispatcher = xValue.GetType ()
                let xConverter = TypeDescriptor.GetConverter xDispatcher
                let xValueStr = xConverter.ConvertTo (xValue, typeof<string>) :?> string
                writer.WriteStartElement xFieldName
                writer.WriteAttributeString ("type", xDispatcher.FullName)
                writer.WriteString xValueStr
                writer.WriteEndElement ()

    /// Write all properties to Xml.
    /// TODO: need a vanilla writeProperties function that writes to an XmlDocument rather than directly to an XmlWriter stream.
    let writePropertiesToXmlWriter (writer : XmlWriter) (source : 'a) =
        let aType = source.GetType ()
        let properties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
        for property in properties do
            if isPropertyWriteable property then
                let propertyValue = property.GetValue source
                match propertyValue with
                | :? Xtension as xtension ->
                    writer.WriteStartElement property.Name
                    writeToXmlWriter writer xtension
                    writer.WriteEndElement ()
                | _ ->
                    let converter = TypeDescriptor.GetConverter property.PropertyType
                    let valueStr = converter.ConvertTo (propertyValue, typeof<string>) :?> string
                    writer.WriteElementString (property.Name, valueStr)