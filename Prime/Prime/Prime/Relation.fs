// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Configuration
open System.ComponentModel
open System.Reflection
open System.Text
open Prime
    
/// Converts Relation types.
type RelationConverter (targetType : Type) =
    inherit TypeConverter ()
    
    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = targetType
        
    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            let toStringMethod = targetType.GetMethod "ToString"
            toStringMethod.Invoke (source, null)
        elif destType = typeof<Symbol> then
            let toStringMethod = targetType.GetMethod "ToString"
            let relationStr = toStringMethod.Invoke (source, null) :?> string
            if Symbol.shouldBeExplicit relationStr then String (relationStr, None) :> obj
            else Atom (relationStr, None) :> obj
        elif destType = targetType then source
        else failconv "Invalid RelationConverter conversion to source." None
        
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = targetType
        
    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as relationStr ->
            let fullName = !!relationStr
            let ftoaFunction = targetType.GetMethod ("makeFromFullName", BindingFlags.Static ||| BindingFlags.Public)
            ftoaFunction.Invoke (null, [|fullName|])
        | :? Symbol as relationSymbol ->
            match relationSymbol with
            | Atom (relationStr, _) | Number (relationStr, _) | String (relationStr, _) -> 
                let fullName = !!relationStr
                let ftoaFunction = targetType.GetMethod ("makeFromFullName", BindingFlags.Static ||| BindingFlags.Public)
                ftoaFunction.Invoke (null, [|fullName|])
            | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Symbol, Number, or String for conversion to Relation." ^ Some relationSymbol
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failconv "Invalid RelationConverter conversion from source." None

[<AutoOpen>]
module RelationModule =

    /// A relation that can be resolved to an address via projection.
    type [<CustomEquality; NoComparison; TypeConverter (typeof<RelationConverter>)>] 'a Relation =
        private
            { OptNames : Name option list
              TypeCarrier : 'a -> unit }
    
        static member internal split (name : Name) =
            Name.split [|'/'|] name
    
        static member internal getFullName (relation : 'a Relation) =
            relation.OptNames |>
            List.map ^ Option.getOrDefault !!"." |>
            Name.join "/"
    
        /// Make a relation from a '/' delimited string where '.' are empty.
        /// NOTE: do not move this function as the RelationConverter's reflection code relies on it being exactly here!
        static member makeFromFullName fullName =
            let namesList = Relation<'a>.split fullName |> List.ofSeq
            let optNames = List.map (fun name -> match Name.getNameStr name with "." -> None | _ -> Some name) namesList
            { OptNames = optNames; TypeCarrier = fun (_ : 'a) -> () }
    
        /// Hash a Relation.
        static member hash (relation : 'a Relation) =
            List.hash relation.OptNames
                
        /// Equate Relations.
        static member equals relation relation2 =
            relation.OptNames = relation2.OptNames
    
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
            Relation<'a>.getFullName this |> Name.getNameStr

    [<RequireQualifiedAccess>]
    module Relation =

        /// Resolve a relationship to an address.
        let resolve<'a> (address : 'a Address) (relation : 'a Relation) =
            let names = List.project id (Address.getNames address) relation.OptNames
            Address.makeFromNames<'a> names
    
        /// Make a relation from a list of option names.
        let makeFromOptNamesList<'a> optNamesList =
            { OptNames = optNamesList |> List.ofSeq; TypeCarrier = fun (_ : 'a) -> () }
    
        /// Make an address from a '/' delimited string.
        let makeFromFullName<'a> fullName =
            Address<'a>.makeFromFullName fullName