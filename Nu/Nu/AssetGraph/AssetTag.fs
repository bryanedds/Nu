// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.ComponentModel
open FSharp.Reflection
open Prime

/// Describes a means for looking up an asset.
type AssetTag =
    abstract PackageName : string
    abstract AssetName : string

/// Converts AssetTag types, interning its strings for look-up speed.
type AssetTagConverter (pointType : Type) =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType.Name = pointType.Name

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let assetTag = source :?> AssetTag
            Symbols
                ([if Symbol.shouldBeExplicit assetTag.PackageName then Text (assetTag.PackageName, ValueNone) else Atom (assetTag.PackageName, ValueNone)
                  if Symbol.shouldBeExplicit assetTag.AssetName then Text (assetTag.AssetName, ValueNone) else Atom (assetTag.AssetName, ValueNone)],
                 ValueNone) :> obj
        elif destType = typedefof<_ AssetTag> then source
        else failconv "Invalid AssetTagConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType.Name = pointType.Name

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([packageNameSymbol; assetNameSymbol], _) ->
                let packageName = match packageNameSymbol with Atom (str, _) | Text (str, _) -> str | _ -> failconv "Expected Atom or Text for package name." (Some packageNameSymbol)
                let assetName = match assetNameSymbol with Atom (str, _) | Text (str, _) -> str | _ -> failconv "Expected Atom or Text for asset name." (Some assetNameSymbol)
                FSharpValue.MakeRecord (pointType, [|String.Intern packageName :> obj; String.Intern assetName|])
            | _ -> failconv "Invalid AssetTagConverter conversion from source." (Some symbol)
        | :? AssetTag -> source
        | _ -> failconv "Invalid AssetTagConverter conversion from source." None

/// Describes a strongly-typed means for looking up an asset.
and [<TypeConverter (typeof<AssetTagConverter>)>] 'a AssetTag =
    { PackageName : string
      AssetName : string }
    interface AssetTag with
        member this.PackageName = this.PackageName
        member this.AssetName = this.AssetName

[<RequireQualifiedAccess>]
module AssetTag =

    /// Check two asset tags for equality.
    let inline equals (left : AssetTag) (right : AssetTag) =
        refEq left right ||
        left.PackageName = right.PackageName &&
        left.AssetName = right.AssetName

    /// Make an asset tag.
    let make<'a> packageName assetName : 'a AssetTag =
        { PackageName = packageName; AssetName = assetName }

    /// Convert an asset tag to a string pair.
    let toPair<'a> (assetTag : AssetTag) =
        (assetTag.PackageName, assetTag.AssetName)

    /// Make an asset tag from a string pair.
    let ofPair<'a> (packageName, assetName) =
        make<'a> packageName assetName

    /// Convert an asset tag with an obj type to one of a specific type.
    let specialize<'a> (assetTag : AssetTag) : 'a AssetTag =
        make<'a> assetTag.PackageName assetTag.AssetName

    /// Check that an asset can utilize dxt5 compression (IE, it's not a normal map or specified as uncompressed)
    /// TODO: move this somewhere more general.
    let dxt5Capable (assetName : string) =
        not (assetName.EndsWith "_n") &&
        not (assetName.EndsWith "_u") &&
        not (assetName.EndsWith "Normal") &&
        not (assetName.EndsWith "Uncompressed")

[<AutoOpen>]
module AssetTagOperators =

    /// Check two asset tags for equality.
    let inline assetEq left right =
        AssetTag.equals left right

    /// Check two asset tags for inequality.
    let inline assetNeq left right =
        not (AssetTag.equals left right)

    /// Make an asset tag.
    let asset<'a> packageName assetName : 'a AssetTag =
        AssetTag.make packageName assetName