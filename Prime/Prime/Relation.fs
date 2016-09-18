// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open System.Reflection
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
        | :? string as fullNameStr ->
            let makeFromStringFunction = targetType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
            let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((targetType.GetGenericArguments ()).[0])
            makeFromStringFunctionGeneric.Invoke (null, [|fullNameStr|])
        | :? Symbol as relationSymbol ->
            match relationSymbol with
            | Atom (fullNameStr, _) | String (fullNameStr, _) ->
                let makeFromStringFunction = targetType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
                let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((targetType.GetGenericArguments ()).[0])
                makeFromStringFunctionGeneric.Invoke (null, [|fullNameStr|])
            | Number (_, _) | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Symbol or String for conversion to Relation." ^ Some relationSymbol
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
    
        /// Make a relation from a '/' delimited string where '.' are empty.
        /// NOTE: do not move this function as the RelationConverter's reflection code relies on it being exactly here!
        static member makeFromString<'a> (relationStr : string) =
            let optNameList = relationStr.Split '/' |> List.ofSeq
            let optNames = List.map (fun name -> match name with "." -> None | _ -> Some !!name) optNameList
            { OptNames = optNames; TypeCarrier = fun (_ : 'a) -> () }

        /// Hash a Relation.
        static member hash (relation : 'a Relation) =
            List.hash relation.OptNames
                
        /// Equate Relations.
        static member equals relation relation2 =
            relation.OptNames = relation2.OptNames

        /// Resolve a relationship to an address.
        static member resolve<'a> (address : 'a Address) (relation : 'a Relation) =
            let names = List.project id (Address.getNames address) relation.OptNames
            Address.makeFromList<'a> names

        /// Concatenate two addresses of the same type.
        static member (+|+) (address : 'a Address, relation : 'a Relation) = Relation.resolve address relation
    
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
            let names = List.map (fun optName -> match optName with Some name -> Name.getNameStr name | None -> ".") this.OptNames
            String.Join ("/", names)

    [<RequireQualifiedAccess>]
    module Relation =

        /// Make a relation from a list of option names.
        let makeFromList<'a> optNamesList =
            { OptNames = optNamesList |> List.ofSeq; TypeCarrier = fun (_ : 'a) -> () }
    
        /// Make a relation from a '/' delimited string.
        let makeFromString<'a> relationStr =
            Relation<'a>.makeFromString relationStr

        /// Get the optional names of a relation.
        let getOptNames relation =
            relation.OptNames

        /// Change the type of an address.
        let changeType<'a, 'b> (relation : 'a Relation) =
            { OptNames = relation.OptNames; TypeCarrier = fun (_ : 'b) -> () }

/// A relation that can be resolved to an address via projection.
type 'a Relation = 'a RelationModule.Relation