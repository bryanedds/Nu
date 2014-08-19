namespace Prime
open System
open System.Xml
open System.Reflection
open System.ComponentModel

[<AutoOpen>]
module OverlayerModule =

    type [<ReferenceEquality>] Overlayer =
        { OverlayDocument : XmlDocument
          RootNode : XmlNode }

[<RequireQualifiedAccess>]
module Overlayer =

    let make (fileName : string) =
        let document = XmlDocument ()
        document.Load fileName
        let root = document.SelectSingleNode "Root"
        { OverlayDocument = document; RootNode = root }

    let rec trySelectNode overlayName propertyName overlayer =
        if String.length overlayName <> 0 && String.length propertyName <> 0 then
            let optBranch = overlayer.RootNode.SelectSingleNode overlayName
            match optBranch with
            | null -> None
            | branch ->
                let optLeaf = branch.SelectSingleNode propertyName
                match optLeaf with
                | null ->
                    let optIncludeNames = branch.SelectSingleNode "Include"
                    match optIncludeNames with
                    | null -> None
                    | includeNames ->
                        let includeNames = includeNames.InnerText.Split ';'
                        let mutable optNode = None
                        let mutable enr = includeNames.GetEnumerator ()
                        while enr.MoveNext () && Option.isNone optNode do
                            let includeName = enr.Current :?> string // must be cast since Array.GetEnumerator is not generic...
                            optNode <- trySelectNode (includeName.Trim ()) propertyName overlayer
                        optNode
                | leaf -> Some leaf
        else None

    let isPropertyOverlaid overlayName propertyName overlayer =
        Option.isSome <| trySelectNode overlayName propertyName overlayer

    let applyOverlayToProperty overlayName (property : PropertyInfo) (target : 'a) overlayer =
        match trySelectNode overlayName property.Name overlayer with
        | None -> ()
        | Some fieldNode -> Xtension.tryReadTargetProperty property fieldNode target

    let applyOverlay overlayName target overlayer =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties (BindingFlags.Public ||| BindingFlags.Instance)
        for property in targetProperties do
            applyOverlayToProperty overlayName property target overlayer

    let inline shouldWriteProperty propertyName target overlayer =
        let targetOptOverlayName = (^a : (member OptOverlayName : string option) target)
        let targetXtension = (^a : (member Xtension : Xtension) target)
        match targetOptOverlayName with
        | None -> true
        | Some overlayName ->
            match trySelectNode overlayName propertyName overlayer with
            | None -> true
            | Some overlayNode ->
                let propertyValue =
                    match (target.GetType ()).GetProperty (propertyName, BindingFlags.Public ||| BindingFlags.Instance) with
                    | null -> Map.find propertyName targetXtension.XFields
                    | targetProperty -> targetProperty.GetValue target
                let converter = TypeDescriptor.GetConverter <| propertyValue.GetType ()
                if converter.CanConvertFrom typeof<string> then
                    let overlayValue = converter.ConvertFrom overlayNode.InnerText
                    overlayValue <> propertyValue
                else true