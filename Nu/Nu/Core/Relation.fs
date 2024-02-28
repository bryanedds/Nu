// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Constants
open System
open Nu

[<RequireQualifiedAccess>]
module Relation =

    let [<Literal>] Current = '~'
    let [<Literal>] CurrentStr = "~"
    let [<Literal>] Parent = '^'
    let [<Literal>] ParentStr = "^"

namespace Nu
open System
open System.ComponentModel
open System.Reflection
open Prime

/// An aspect of a relation.
type Link =
    | Current
    | Parent
    | Name of string

/// Converts Relation types.
type RelationConverter (pointType : Type) =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            let toStringMethod = pointType.GetMethod "ToString"
            toStringMethod.Invoke (source, null)
        elif destType = typeof<Symbol> then
            let toStringMethod = pointType.GetMethod "ToString"
            let relationStr = toStringMethod.Invoke (source, null) :?> string
            if Symbol.shouldBeExplicit relationStr then Text (relationStr, ValueNone) :> obj
            else Atom (relationStr, ValueNone) :> obj
        elif destType = pointType then source
        else failconv "Invalid RelationConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as addressStr ->
            let makeFromStringFunction = pointType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
            let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((pointType.GetGenericArguments ()).[0])
            makeFromStringFunctionGeneric.Invoke (null, [|addressStr|])
        | :? Symbol as relationSymbol ->
            match relationSymbol with
            | Atom (addressStr, _) | Text (addressStr, _) ->
                let makeFromStringFunction = pointType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
                let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((pointType.GetGenericArguments ()).[0])
                makeFromStringFunctionGeneric.Invoke (null, [|addressStr|])
            | Number (_, _) | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Atom or Text for conversion to Relation." (Some relationSymbol)
        | _ ->
            if pointType.IsInstanceOfType source then source
            else failconv "Invalid RelationConverter conversion from source." None

[<AutoOpen>]
module Relation =

    /// A relation that can be resolved to an address via contextual resolution.
    type [<CustomEquality; NoComparison; TypeConverter (typeof<RelationConverter>)>] 'a Relation =
        { Links : Link array }

        /// Make a relation from an array of names.
        static member makeFromArray<'a> (names : string array) : 'a Relation =
            let links =
                Array.map (fun name ->
                    match name with
                    | Constants.Relation.CurrentStr -> Current
                    | Constants.Relation.ParentStr -> Parent
                    | _ -> Name name)
                    names
            { Links = links }

        /// Make a relation from a list of names.
        static member makeFromList<'a> (names : string list) : 'a Relation =
            Relation.makeFromArray<'a> (List.toArray names)

        /// Make a relation from an address.
        static member makeFromAddress<'a> (address : 'a Address) : 'a Relation =
            let names = Address.getNames address
            Relation.makeFromArray<'a> names

        /// Make a relation from a '/' delimited string.
        /// NOTE: do not move this function as the RelationConverter's reflection code relies on it being exactly here!
        static member makeFromString<'a> (relationStr : string) : 'a Relation =
            let names = relationStr.Split Constants.Address.Separator
            Relation.makeFromArray<'a> names

        /// Hash a Relation.
        static member hash (relation : 'a Relation) =
            Array.hash relation.Links

        /// Equate Relations.
        static member equals (relation : 'a Relation) (relation2 : 'a Relation) =
            refEq relation relation2 || // OPTIMIZATION: first check ref equality
            seqEq relation.Links relation2.Links

        /// Resolve a relation from an address.
        static member resolve<'a, 'b> (address : 'a Address) (relation : 'b Relation) : 'b Address =
            // TODO: optimize this with hand-written code.
            let addressStr = string address
            let relationStr = string relation
            let pathStr = relationStr.Replace("^", "..").Replace('~', '.')
            let resultStr =
                addressStr + Constants.Address.SeparatorStr + pathStr |>
                (fun path -> Uri(Uri("http://example.com/"), path).AbsolutePath.TrimStart('/')) |>
                Uri.UnescapeDataString
            let resultStr =
                let resultStrLen = resultStr.Length
                if resultStrLen > 0 && resultStr.[dec resultStrLen] = '/'
                then resultStr.Substring (0, dec resultStrLen)
                else resultStr
            let result = Address.makeFromString resultStr
            result

        /// Relate the second address to the first.
        static member relate<'a, 'b> (address : 'a Address) (address2 : 'b Address) : 'b Relation =
            // TODO: P1: use Uri.MakeRelativeUri here instead of this likely screwed up algorithm -
            // https://stackoverflow.com/a/1766773/1082782
            let names = Address.getNames address
            let names2 = Address.getNames address2
            let namesMatching =
                let mutable namesMatching = 0
                let mutable enr = (names :> _ seq).GetEnumerator ()
                let mutable enr2 = (names2 :> _ seq).GetEnumerator ()
                while (enr.MoveNext() && enr2.MoveNext ()) do
                    if enr.Current = enr2.Current then
                        namesMatching <- inc namesMatching
                namesMatching
            let names3 = Array.trySkip namesMatching names2
            match names3 with
            | [||] ->
                { Links = [|Current|] }
            | _ ->
                let parents = Array.init (names.Length - namesMatching) (fun _ -> Parent)
                let links = Array.map Name names3
                { Links = Array.append parents links }

        interface 'a Relation IEquatable with
            member this.Equals that =
                Relation<'a>.equals this that

        override this.Equals that =
            match that with
            | :? ('a Relation) as that -> Relation<'a>.equals this that
            | _ -> false

        override this.GetHashCode () =
            Relation<'a>.hash this

        override this.ToString () =
            let names =
                Array.map (fun link ->
                    match link with
                    | Current -> Constants.Relation.CurrentStr
                    | Parent -> Constants.Relation.ParentStr
                    | Name name -> name)
                    this.Links
            String.concat Constants.Address.SeparatorStr names

    [<RequireQualifiedAccess>]
    module Relation =

        /// Make a relation from a list of option names.
        let makeFromArray<'a> links : 'a Relation =
            { Links = links }

        /// Make a relation from a list of option names.
        let makeFromList<'a> links : 'a Relation =
            { Links = List.toArray links }

        /// Make a relation from a '/' delimited string.
        let makeFromString<'a> relationStr =
            Relation<'a>.makeFromString relationStr

        /// Make a current relation.
        let makeCurrent () =
            Relation.makeFromArray [|Constants.Relation.CurrentStr|]

        /// Make a parent relation.
        let makeParent () =
            Relation.makeFromArray [|Constants.Relation.ParentStr|]

        /// Test relation equality.
        let equals (left : 'a Relation) (right : 'a Relation) =
            Relation<'a>.equals left right

        /// Get the links of a relation.
        let getLinks relation =
            relation.Links

        /// Change the type of an address.
        let changeType<'a, 'b> (relation : 'a Relation) : 'a Relation =
            { Links = relation.Links }

/// A relation that can be resolved to an address via projection.
type 'a Relation = 'a Relation.Relation