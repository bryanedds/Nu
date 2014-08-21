// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open System.Reflection
open System.ComponentModel
open System.Xml
open Microsoft.FSharp.Reflection
open Prime
open Prime.PrimeConstants

[<AutoOpen>]
module XtensionModule =

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
    type XFields =
        Map<string, obj>

    /// Xtensions (and their supporting types) are a dynamic, functional, and semi-convenient way
    /// to solve the 'expression problem' in F#, and can also be used to implement a dynamic
    /// 'Entity-Component System'.
    type [<StructuralEquality; NoComparison>] Xtension =
        { XFields : XFields
          OptXDispatcherName : string option
          CanDefault : bool
          Sealed : bool }

        // NOTE: this could instead be a special class with a MethodMissing method
        static member private emptyDispatcher = new obj ()

        static member private getDefaultValue () : 'r =
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
                    if converter.CanConvertFrom defaultValueType then converter.ConvertFrom defaultValue :?> 'r
                    else failwith <| "Cannot convert '" + string defaultValue + "' to type '" + defaultFieldType.Name + "'."

        static member private tryGetDefaultValue (this : Xtension) memberName : 'r =
            if this.CanDefault then Xtension.getDefaultValue ()
            else failwith <| "The Xtension field '" + memberName + "' does not exist and no default is permitted because CanDefault is false."

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

        static member dispatchesAs2 (dispatcherTargetType : Type) dispatcher =
            let dispatcherType = dispatcher.GetType ()
            let result =
                dispatcherTargetType = dispatcherType ||
                dispatcherType.IsSubclassOf dispatcherTargetType
            result

        static member dispatchesAs (dispatcherTargetType : Type) xtension dispatcherContainer =
            let dispatcher = Xtension.getDispatcher xtension dispatcherContainer
            Xtension.dispatchesAs2 dispatcherTargetType dispatcher

        /// The dynamic dispatch operator.
        /// TODO: search for a way to effectively optimize this function, especially in regard
        /// to its use of MethodInfo.Invoke.
        static member (?) (xtension, memberName) : 'a -> 'r =

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

                // return field directly if the return type matches, otherwise the default value for that type
                | Some field ->
                    match field with
                    | :? 'r as fieldValue -> fieldValue
                    | _ -> Xtension.tryGetDefaultValue xtension memberName

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
        not <| propertyName.EndsWith "Ns" // 'Ns' stands for 'Not serializable'

    /// Is the given property writable?
    let isPropertyWriteable (property : PropertyInfo) =
        property.CanWrite &&
        isPropertyNameWriteable property.Name

    /// Read an Xtension's fields from Xml.
    let readXFields (valueNode : XmlNode) =
        let childNodes = enumerable valueNode.ChildNodes
        Seq.map
            (fun (xNode : XmlNode) ->
                let typeName = xNode.Attributes.[TypeAttributeName].InnerText
                let aType = findType typeName
                let xValueStr = xNode.InnerText
                let converter = TypeDescriptor.GetConverter aType
                if converter.CanConvertFrom typeof<string> then (xNode.Name, converter.ConvertFrom xValueStr)
                else failwith <| "Cannot convert string '" + xValueStr + "' to type '" + typeName + "'.")
            childNodes

    /// Read an optXDispatcherName from an xml node.
    let readOptXDispatcherName (node : XmlNode) =
        match node.Attributes.[OptXDispatcherNameAttributeName] with
        | null -> None
        | optXDispatcherNameAttribute ->
            let optXDispatcherNameStr = optXDispatcherNameAttribute.InnerText
            let optStrConverter = TypeDescriptor.GetConverter typeof<string option>
            optStrConverter.ConvertFrom optXDispatcherNameStr :?> string option

    /// Read an Xtension from Xml.
    let read valueNode =
        let xFields = Map.ofSeq <| readXFields valueNode
        let optXDispatcherName = readOptXDispatcherName valueNode
        { XFields = xFields; OptXDispatcherName = optXDispatcherName; CanDefault = true; Sealed = false }

    /// Attempt to read a target's property from Xml.
    let tryReadTargetProperty (property : PropertyInfo) (valueNode : XmlNode) (target : 'a) =
        if property.PropertyType = typeof<Xtension> then
            let xtension = property.GetValue target :?> Xtension
            let xFields = readXFields valueNode
            let optXDispatcherName = readOptXDispatcherName valueNode
            let xtension = { xtension with XFields = Map.addMany xFields xtension.XFields; OptXDispatcherName = optXDispatcherName }
            property.SetValue (target, xtension)
        else
            let valueStr = valueNode.InnerText
            let converter = TypeDescriptor.GetConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFrom valueStr
                property.SetValue (target, value)

    /// Read a target's property from Xml if possible.
    let readTargetProperty (fieldNode : XmlNode) (target : 'a) =
        match typeof<'a>.GetPropertyWritable (fieldNode.Name, BindingFlags.Public ||| BindingFlags.Instance) with
        | None -> ()
        | Some property -> tryReadTargetProperty property fieldNode target

    /// Read all of a target's properties from Xml.
    let readTargetProperties (targetNode : XmlNode) target =
        for node in targetNode.ChildNodes do
            readTargetProperty node target

    /// Read just the target's XDispatcher from Xml.
    let readTargetXDispatcher (targetNode : XmlNode) target =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties (BindingFlags.Public ||| BindingFlags.Instance)
        let xtensionProperty = Array.find (fun (property : PropertyInfo) -> property.PropertyType = typeof<Xtension> && isPropertyWriteable property) targetProperties
        let xtensionNode = targetNode.[xtensionProperty.Name]
        let optXDispatcherName = readOptXDispatcherName xtensionNode
        let xtension = xtensionProperty.GetValue target :?> Xtension
        let xtension = { xtension with OptXDispatcherName = optXDispatcherName }
        xtensionProperty.SetValue (target, xtension)

    /// Write an Xtension to Xml.
    // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
    /// XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
    let write shouldWriteProperty (writer : XmlWriter) xtension =
        writer.WriteAttributeString (OptXDispatcherNameAttributeName, string xtension.OptXDispatcherName)
        for xField in xtension.XFields do
            let xFieldName = xField.Key
            if  isPropertyNameWriteable xFieldName &&
                shouldWriteProperty xFieldName then
                let xValue = xField.Value
                let xDispatcher = xValue.GetType ()
                let xConverter = TypeDescriptor.GetConverter xDispatcher
                let xValueStr = xConverter.ConvertTo (xValue, typeof<string>) :?> string
                writer.WriteStartElement xFieldName
                writer.WriteAttributeString (TypeAttributeName, xDispatcher.FullName)
                writer.WriteString xValueStr
                writer.WriteEndElement ()

    /// Write all of a target's properties to Xml.
    // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
    /// XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
    let writeTargetProperties shouldWriteProperty (writer : XmlWriter) (source : 'a) =
        let aType = source.GetType ()
        let properties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
        for property in properties do
            let propertyValue = property.GetValue source
            match propertyValue with
            | :? Xtension as xtension ->
                writer.WriteStartElement property.Name
                write shouldWriteProperty writer xtension
                writer.WriteEndElement ()
            | _ ->
                if  isPropertyWriteable property &&
                    shouldWriteProperty property.Name then
                    let converter = TypeDescriptor.GetConverter property.PropertyType
                    let valueStr = converter.ConvertTo (propertyValue, typeof<string>) :?> string
                    writer.WriteElementString (property.Name, valueStr)