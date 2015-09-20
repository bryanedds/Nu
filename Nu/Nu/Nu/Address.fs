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

/// Converts Address types.
type AddressConverter (targetType : Type) =
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
            let ftoaFunction = targetType.GetMethod ("ftoa", BindingFlags.Static ||| BindingFlags.Public)
            ftoaFunction.Invoke (null, [|source|])
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failwith "Invalid AddressConverter conversion from source."

/// Specifies the address of an element in a game, or name of an event.
/// OPTIMIZATION: NameKeys are used in case we can manage some sort of hashing look-up with them.
/// OPTIMIZATION: Comparison is done using the full string of names for speed.
/// OPTIMIZATION: OptFullName and OptHashCode are lazy for speed.
and [<CustomEquality; CustomComparison; TypeConverter (typeof<AddressConverter>)>] 'a Address =
    private
        { NameKeys : NameKey list
          mutable OptFullName : string option
          mutable OptHashCode : int option
          TypeCarrier : 'a -> unit }

    static member internal join (seq : string seq) =
        String.Join ("/", seq)

    static member internal split (str : string) =
        List.ofArray ^ str.Split '/'

    static member internal getFullName (address : 'a Address) =
        match address.OptFullName with
        | Some fullName -> fullName
        | None ->
            let fullName = address.NameKeys |> Seq.map (fun nameKey -> nameKey.Name) |> Address<'a>.join
            address.OptFullName <- Some fullName
            fullName

    /// Hash an Address.
    static member hash (address : 'a Address) =
        match address.OptHashCode with
        | Some hashCode -> hashCode
        | None ->
            let hashCode = let fullName = Address<'a>.getFullName address in fullName.GetHashCode ()
            address.OptHashCode <- Some hashCode
            hashCode
            
    /// Equate Addresses.
    static member equals address address2 =
        let addressName = Address<'a>.getFullName address
        let address2Name = Address<'a>.getFullName address2
        String.Equals (addressName, address2Name, StringComparison.Ordinal)

    /// Compare Addresses.
    static member compare address address2 =
        String.Compare (Address<'a>.getFullName address, Address<'a>.getFullName address2, StringComparison.Ordinal)

    interface 'a Address IComparable with
        member this.CompareTo that =
            Address<'a>.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? ('a Address) as that -> Address<'a>.compare this that
            | _ -> failwith "Invalid Address comparison (comparee not of type Address)."

    interface 'a Address IEquatable with
        member this.Equals that =
            Address<'a>.equals this that

    override this.Equals that =
        match that with
        | :? ('a Address) as that -> Address<'a>.equals this that
        | _ -> false

    override this.GetHashCode () =
        Address<'a>.hash this
    
    override this.ToString () =
        Address<'a>.getFullName this

[<RequireQualifiedAccess>]
module Address =

    /// The empty address.
    let empty<'a> =
        { NameKeys = []; OptFullName = Some String.Empty; OptHashCode = Some 0; TypeCarrier = fun (_ : 'a) -> () }

    /// Make an address from name keys.
    let makeFromNameKeys<'a> nameKeys =
        { NameKeys = nameKeys; OptFullName = None; OptHashCode = None; TypeCarrier = fun (_ : 'a) -> () }

    /// Make an address from a list of names.
    let makeFromNamesList<'a> namesList =
        let nameKeys = List.map NameKey.make namesList
        makeFromNameKeys<'a> nameKeys

    /// Make an address from a '/' delimited string.
    let makeFromFullName<'a> fullName =
        let namesList = Address<'a>.split fullName
        let nameKeys = List.map NameKey.make namesList
        { NameKeys = nameKeys; OptFullName = Some fullName; OptHashCode = None; TypeCarrier = fun (_ : 'a) -> () }

    /// Get the name keys of an address.
    let getNameKeys address =
        address.NameKeys

    /// Change the type of an address.
    let changeType<'a, 'b> (address : 'a Address) =
        { NameKeys = address.NameKeys; OptFullName = None; OptHashCode = None; TypeCarrier = fun (_ : 'b) -> () }

    /// Get the full name of an address as a single string.
    let getFullName address =
        Address<'a>.getFullName address

    /// Get the name of an address as a string.
    let getName address =
        getNameKeys address |> List.last |> NameKey.getName

    /// Get the address's hash code.
    let getHashCode address =
        Address<'a>.hash address

    /// Take the head of an address.
    let head address =
        List.head address.NameKeys
        
    /// Take the tail of an address.
    let tail<'a> address =
        makeFromNameKeys<'a> ^ List.tail address.NameKeys

    /// Take a name key of an address.
    let at index address =
        List.at index address.NameKeys

    /// Take an address composed of the name keys of an address minus a skipped amount of name keys.
    let skip<'a, 'b> n (address : 'a Address) =
        makeFromNameKeys<'b> ^ List.skip n address.NameKeys

    /// Take an address composed of the given number of name keys of an address.
    let take<'a, 'b> n (address : 'a Address) =
        makeFromNameKeys<'b> ^ List.take n address.NameKeys

    /// Take the last name key of an address.
    let last address =
        List.last address.NameKeys

    /// Take an address composed of all but the last name of an address.
    let allButLast<'a, 'b> (address : 'a Address) =
        makeFromNameKeys<'b> ^ List.allButLast address.NameKeys

    /// Get the length of an address by its name keys.
    let length address =
        List.length address.NameKeys

    /// Query that an address is devoid of name keys.
    let isEmpty address =
        List.isEmpty address.NameKeys

[<AutoOpen>]
module AddressOperators =

    /// Convert an address of type 'a to an address of type 'b.
    let atoa<'a, 'b> address =
        Address.changeType<'a, 'b> address

    /// Convert a name keys into an address.
    let ktoa<'a> nameKeys =
        Address.makeFromNameKeys<'a> nameKeys

    /// Convert a names list into an address.
    let ltoa<'a> namesList =
        Address.makeFromNamesList<'a> namesList

    /// Convert a full name into an address.
    let ftoa<'a> fullName =
        Address.makeFromFullName<'a> fullName

    /// Convert a single name into an address.
    let ntoa<'a> nameStr =
        ltoa<'a> [nameStr]

    /// Convert any address to an obj Address.
    let atooa<'a> (address : 'a Address) =
        { NameKeys = address.NameKeys; OptFullName = None; OptHashCode = None; TypeCarrier = fun (_ : obj) -> () }

    /// Concatenate two addresses of the same type.
    let acat<'a> (address : 'a Address) (address2 : 'a Address) =
        ktoa<'a> (address.NameKeys @ address2.NameKeys)

    /// Concatenate two addresses, taking the type of first address.
    let acatf<'a> (address : 'a Address) (address2 : obj Address) =
        ktoa<'a> (address.NameKeys @ address2.NameKeys)
    
    /// Concatenate two addresses, forcing the type of first address.
    let acatff<'a, 'b> (address : 'a Address) (address2 : 'b Address) =
        acatf address ^ atooa address2

    /// Concatenate two addresses, taking the type of the second address.
    let acats<'a> (address : obj Address) (address2 : 'a Address) =
        ktoa<'a> (address.NameKeys @ address2.NameKeys)
    
    /// Concatenate two addresses, forcing the type of second address.
    let acatsf<'a, 'b> (address : 'a Address) (address2 : 'b Address) =
        acats (atooa address) address2

/// Implement operators as static members.
type Address with

    /// Concatenate two addresses of the same type.
    static member (-|-) (address, address2) = acat address address2

    /// Concatenate two addresses, taking the type of first address.
    static member (->-) (address, address2) = acatf address address2

    /// Concatenate two addresses, forcing the type of first address.
    static member (->>-) (address, address2) = acatff address address2

    /// Concatenate two addresses, taking the type of the second address.
    static member (-<-) (address, address2) = acats address address2

    /// Concatenate two addresses, forcing the type of second address.
    static member (-<<-) (address, address2) = acatsf address address2