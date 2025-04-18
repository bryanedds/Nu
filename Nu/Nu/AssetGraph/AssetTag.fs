// Nu Game Engine.
// Copyright (C) Bryan Edds.

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
/// TODO: P1: implement ('a AssetTag) IComparable as well as it might make it more efficient.
and [<CustomEquality; CustomComparison; TypeConverter (typeof<AssetTagConverter>)>] 'a AssetTag =
    { PackageName : string
      AssetName : string }

    member this.Pair =
        (this.PackageName, this.AssetName)

    override this.GetHashCode() =
        this.PackageName.GetHashCode () ^^^ this.AssetName.GetHashCode ()

    override this.Equals that =
        refEq (this :> obj) that ||
        match that with
        | :? AssetTag as thatTag -> this.PackageName = thatTag.PackageName && this.AssetName = thatTag.AssetName
        | _ -> false

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? AssetTag as thatTag ->
                let packageComparison = String.CompareOrdinal (this.PackageName, thatTag.PackageName)
                if packageComparison <> 0 then packageComparison
                else String.CompareOrdinal (this.AssetName, thatTag.AssetName)
            | _ -> failwith "Cannot compare Address (comparee not of type AssetTag)."

    interface AssetTag with
        member this.PackageName = this.PackageName
        member this.AssetName = this.AssetName

[<RequireQualifiedAccess>]
module AssetTag =

    /// Make an asset tag.
    let make<'a> packageName assetName : 'a AssetTag =
        { PackageName = packageName; AssetName = assetName }

    /// Make an asset tag from a string.
    let makeFromString<'a> str : 'a AssetTag =
        scvalue str

    /// Make a empty asset tag.
    let makeEmpty<'a> () =
        make<'a> "" ""

    /// Convert an asset tag to a string pair.
    let toPair<'a> (assetTag : AssetTag) =
        (assetTag.PackageName, assetTag.AssetName)

    /// Make an asset tag from a string pair.
    let ofPair<'a> (packageName, assetName) =
        make<'a> packageName assetName

    /// Convert an asset tag with an obj type to one of a specific type.
    let specialize<'a> (assetTag : AssetTag) : 'a AssetTag =
        make<'a> assetTag.PackageName assetTag.AssetName

[<AutoOpen>]
module AssetTagOperators =

    /// Check two asset tags for equality.
    let inline assetEq (left : AssetTag) (right : AssetTag) =
        left = right

    /// Check two asset tags for inequality.
    let inline assetNeq left right =
        not (assetEq left right)

    /// Make an asset tag.
    let asset<'a> packageName assetName : 'a AssetTag =
        AssetTag.make packageName assetName