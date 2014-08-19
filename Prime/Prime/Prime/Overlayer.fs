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
                    let optParentNames = branch.SelectSingleNode "Parentage"
                    match optParentNames with
                    | null -> None
                    | parentNames ->
                        let parentNames = parentNames.InnerText.Split ';'
                        let mutable optNode = None
                        let mutable enr = parentNames.GetEnumerator ()
                        while enr.MoveNext () && Option.isNone optNode do
                            let parentName = enr.Current :?> string // must be cast since Array.GetEnumerator is not generic...
                            optNode <- trySelectNode (parentName.Trim ()) propertyName overlayer
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