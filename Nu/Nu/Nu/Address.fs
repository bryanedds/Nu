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

/// A name key for optimized look-up in hashing containers.
/// OPTIMIZATION: OptHashCode is lazy for speed.
type [<CustomEquality; NoComparison>] NameKey =
    { Name : string
      mutable OptHashCode : int option }

    interface NameKey IEquatable with
        member this.Equals that =
            this.Name = that.Name

    override this.Equals that =
        match that with
        | :? NameKey as that -> this.Name = that.Name
        | _ -> false

    override this.GetHashCode () =
        match this.OptHashCode with
        | Some hashCode -> hashCode
        | None ->
            let hashCode = hash this.Name
            this.OptHashCode <- Some hashCode
            hashCode

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NameKey =

    /// Make a name key from a single address name string.
    let make addressName =
        { Name = addressName
          OptHashCode = None }

/// Specifies the address of an element in a game, or name of an event.
/// OPTIMIZATION: NameKeys are used in case we can manage some sort of hashing look-up with them.
/// OPTIMIZATION: Comparison is done using the full string of names for speed.
/// OPTIMIZATION: OptNamesStr and OptHashCode are lazy for speed.
type [<CustomEquality; CustomComparison; TypeConverter (typeof<AddressConverter>)>] 'a Address =
    { NameKeys : NameKey list
      mutable OptNamesStr : string option
      mutable OptHashCode : int option
      TypeCarrier : 'a -> unit }

    static member internal join (seq : string seq) =
        String.Join ("/", seq)

    static member internal split (str : string) =
        List.ofArray <| str.Split '/'

    /// The empty address.
    static member empty =
        { NameKeys = []; OptNamesStr = Some String.Empty; OptHashCode = Some 0; TypeCarrier = fun (_ : 'a) -> () }

    /// Make an address from name keys.
    static member makeFromNameKeys nameKeys =
        { NameKeys = nameKeys; OptNamesStr = None; OptHashCode = None; TypeCarrier = fun (_ : 'a) -> () }

    /// Make an address from a list of names.
    static member makeFromNamesList namesList =
        let nameKeys = List.map NameKey.make namesList
        Address<'a>.makeFromNameKeys nameKeys

    /// Make an address from a '/' delimited string.
    static member makeFromNamesString namesStr =
        let namesList = Address<'a>.split namesStr
        let nameKeys = List.map NameKey.make namesList
        { NameKeys = nameKeys; OptNamesStr = Some namesStr; OptHashCode = None; TypeCarrier = fun (_ : 'a) -> () }

    /// Convert a names string into an address.
    static member stoa (namesStr : string) =
        Address<'a>.makeFromNamesString namesStr

    /// TODO: document!
    static member getNamesStr (address : 'a Address) =
        match address.OptNamesStr with
        | Some namesStr -> namesStr
        | None ->
            let namesStr = address.NameKeys |> Seq.map (fun nameKey -> nameKey.Name) |> Address<'a>.join
            address.OptNamesStr <- Some namesStr
            namesStr

    /// TODO: document!
    static member getHashCode (address : 'a Address) =
        match address.OptHashCode with
        | Some hashCode -> hashCode
        | None ->
            let hashCode = hash <| Address<'a>.getNamesStr address
            address.OptHashCode <- Some hashCode
            hashCode

    interface 'a Address IComparable with
        member this.CompareTo that =
            String.Compare (Address<'a>.getNamesStr this, Address<'a>.getNamesStr that)

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? ('a Address) as that -> String.Compare (Address<'a>.getNamesStr this, Address<'a>.getNamesStr that)
            | _ -> failwith "Invalid Address comparison (comparee not of type Address)."

    interface 'a Address IEquatable with
        member this.Equals that =
            Address<'a>.getNamesStr this = Address<'a>.getNamesStr that

    override this.Equals that =
        match that with
        | :? ('a Address) as that -> Address<'a>.getNamesStr this = Address<'a>.getNamesStr that
        | _ -> false

    override this.GetHashCode () =
        Address<'a>.getHashCode this
    
    override this.ToString () =
        Address<'a>.getNamesStr this

/// Converts Address types.
and AddressConverter (targetType : Type) =
    inherit TypeConverter ()
    
    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = targetType
        
    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            let toStringMethod = targetType.GetMethod "ToString"
            toStringMethod.Invoke (source, null)
        elif destType = targetType then source
        else failwith "Invalid AddressConverter conversion to source."
        
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = targetType
        
    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string ->
            let stoaFunction = targetType.GetMethod ("stoa", BindingFlags.Static ||| BindingFlags.Public)
            stoaFunction.Invoke (null, [|source|])
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failwith "Invalid AddressConverter conversion from source."

[<AutoOpen>]
module AddressModule =

    /// Convert a names string into an address.
    let stoa<'a> namesStr =
        Address<'a>.stoa namesStr

    /// Convert a name keys into an address.
    let ktoa<'a> nameKeys =
        Address<'a>.makeFromNameKeys nameKeys

    /// Convert a names list into an address.
    let ltoa<'a> namesList =
        Address<'a>.makeFromNamesList namesList

    /// Convert a name string into an address.
    let ntoa<'a> nameStr =
        ltoa<'a> [nameStr]

    /// Convert any address to an obj Address.
    let atooa<'a> (address : 'a Address) =
        { NameKeys = address.NameKeys; OptNamesStr = None; OptHashCode = None; TypeCarrier = fun (_ : obj) -> () }

    /// Concatenate two addresses of the same type.
    let acat<'a> (address : 'a Address) (address2 : 'a Address) =
        Address<'a>.makeFromNameKeys (address.NameKeys @ address2.NameKeys)

    /// Concatenate two addresses, taking the type of first address.
    let acatf<'a> (address : 'a Address) (address2 : obj Address) =
        Address<'a>.makeFromNameKeys (address.NameKeys @ address2.NameKeys)
    
    /// Concatenate two addresses, forcing the type of first address.
    let acatff<'a, 'b> (address : 'a Address) (address2 : 'b Address) =
        acatf address <| atooa address2

    /// Concatenate two addresses, taking the type of the second address.
    let acats<'a> (address : obj Address) (address2 : 'a Address) =
        Address<'a>.makeFromNameKeys (address.NameKeys @ address2.NameKeys)
    
    /// Concatenate two addresses, forcing the type of second address.
    let acatsf<'a, 'b> (address : 'a Address) (address2 : 'b Address) =
        acats (atooa address) address2
    
    /// Concatenate two addresses of the same type.
    let (-|-) = acat

    /// Concatenate two addresses, taking the type of first address.
    let (->-) = acatf

    /// Concatenate two addresses, forcing the type of first address.
    let (->>-) = acatff

    /// Concatenate two addresses, taking the type of the second address.
    let (-<-) = acats
    
    /// Concatenate two addresses, forcing the type of second address.
    let (-<<-) = acatsf

[<RequireQualifiedAccess>]
module Address =

    /// Change the type of an address.
    let changeType<'a, 'b> (address : 'a Address) =
        { NameKeys = address.NameKeys; OptNamesStr = None; OptHashCode = None; TypeCarrier = fun (_ : 'b) -> () }

    /// TODO: document!
    let getNamesStr address =
        Address<'a>.getNamesStr address

    /// TODO: document!
    let getHashCode address =
        Address<'a>.getHashCode address

    /// Take the head of an address.
    let head address =
        List.head address.NameKeys
        
    /// Take the tail of an address.
    let tail<'a> address =
        Address<'a>.makeFromNameKeys <| List.tail address.NameKeys

    /// Take a name key of an address.
    let at index address =
        List.at index address.NameKeys

    /// Take an address composed of the name keys of an address minus a skipped amount of name keys.
    let skip<'a, 'b> n (address : 'a Address) =
        Address<'b>.makeFromNameKeys <| List.skip n address.NameKeys

    /// Take an address composed of the given number of name keys of an address.
    let take<'a, 'b> n (address : 'a Address) =
        Address<'b>.makeFromNameKeys <| List.take n address.NameKeys

    /// Take the last name key of an address.
    let last address =
        List.last address.NameKeys

    /// Take an address composed of all but the last name of an address.
    let allButLast<'a, 'b> (address : 'a Address) =
        Address<'b>.makeFromNameKeys <| List.allButLast address.NameKeys

    /// Get the length of an address by its name keys.
    let length address =
        List.length address.NameKeys

    /// Query that an address is devoid of name keys.
    let isEmpty address =
        List.isEmpty address.NameKeys