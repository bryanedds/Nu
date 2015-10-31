// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Configuration
open System.ComponentModel
open System.Reflection
open System.Text
open Prime
open Nu
    
/// Converts Relation types.
type RelationConverter (targetType : Type) =
    inherit TypeConverter ()
    
    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = targetType
        
    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            let toStringMethod = targetType.GetMethod "ToString"
            toStringMethod.Invoke (source, null)
        elif destType = targetType then source
        else failwith "Invalid RelationConverter conversion to source."
        
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = targetType
        
    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string ->
            let ftoaFunction = targetType.GetMethod ("makeFromFullName", BindingFlags.Static ||| BindingFlags.Public)
            ftoaFunction.Invoke (null, [|source|])
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failwith "Invalid RelationConverter conversion from source."

/// A relation that can be resolved to an address via projection.
/// TODO: make use Name instead of NameKey, and move into Prime.
type [<CustomEquality; NoComparison; TypeConverter (typeof<RelationConverter>)>] 'a Relation =
    private
        { OptNameKeys : NameKey option list
          TypeCarrier : 'a -> unit }

    static member internal split (str : string) =
        List.ofArray ^ str.Split '/'

    static member internal getFullName (relation : 'a Relation) =
        relation.OptNameKeys |>
        List.map (fun optNameKey -> match optNameKey with Some nameKey -> nameKey.Name | None -> ".") |>
        List.join "/"

    /// Make a relation from a '/' delimited string where '.' are empty.
    /// NOTE: do not move this function as the RelationConverter's reflection code relies on it being exactly here!
    static member makeFromFullName fullName =
        let namesList = Relation<'a>.split fullName
        let optNameKeys = List.map (fun name -> match name with "." -> None | _ -> Some ^ NameKey.make name) namesList
        { OptNameKeys = optNameKeys; TypeCarrier = fun (_ : 'a) -> () }

    /// Hash a Relation.
    static member hash (relation : 'a Relation) =
        List.hash relation.OptNameKeys
            
    /// Equate Relations.
    static member equals relation relation2 =
        relation.OptNameKeys = relation2.OptNameKeys

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
        Relation<'a>.getFullName this

[<RequireQualifiedAccess>]
module Relation =
    
    /// Resolve a relationship to an address.
    let resolve<'a> (address : 'a Address) (relation : 'a Relation) =
        let nameKeys = List.project id address.NameKeys relation.OptNameKeys
        Address.makeFromNameKeys<'a> nameKeys

    /// Make a relation from a list of option names.
    let makeFromOptNamesList<'a> optNamesList =
        let optNameKeys = List.map (Option.map NameKey.make) optNamesList
        { OptNameKeys = optNameKeys; TypeCarrier = fun (_ : 'a) -> () }

    /// Make an address from a '/' delimited string.
    let makeFromFullName<'a> fullName =
        Address<'a>.makeFromFullName fullName