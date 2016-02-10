// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Configuration
open System.ComponentModel
open System.Reflection
open System.Text
open Prime

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
        | :? string as addressStr ->
            let fullName = Name.make addressStr
            let ftoaFunction = targetType.GetMethod ("makeFromFullName", BindingFlags.Static ||| BindingFlags.Public)
            ftoaFunction.Invoke (null, [|fullName|])
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failwith "Invalid AddressConverter conversion from source."

/// Specifies the address of an identifiable value.
/// OPTIMIZATION: HashCode is cached for speed.
type [<CustomEquality; CustomComparison; TypeConverter (typeof<AddressConverter>)>] 'a Address =
    private
        { Names : Name list
          HashCode : int
          TypeCarrier : 'a -> unit }

    static member internal join (names : Name seq) =
        Name.join "/" names

    static member internal split (name : Name) =
        Name.split [|'/'|] name

    static member internal getFullName (address : 'a Address) =
        Address<'a>.join address.Names

    /// Make an address from a '/' delimited string.
    /// NOTE: do not move this function as the AddressConverter's reflection code relies on it being exactly here!
    static member makeFromFullName fullName =
        let names = Address<'a>.split fullName |> List.ofSeq
        { Names = names; HashCode = Name.hashNames names; TypeCarrier = fun (_ : 'a) -> () }

    /// Hash an Address.
    static member hash (address : 'a Address) =
        address.HashCode
            
    /// Equate Addresses.
    static member equals address address2 =
        Name.equateNames address.Names address2.Names

    /// Compare Addresses.
    static member compare address address2 =
        Name.compareNames address.Names address2.Names

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
        Address<'a>.getFullName this |> Name.getNameStr

[<RequireQualifiedAccess>]
module Address =

    /// The empty address.
    let empty<'a> =
        { Names = []; HashCode = 0; TypeCarrier = fun (_ : 'a) -> () }

    /// Make an address from names.
    let makeFromNames<'a> names =
        { Names = names |> List.ofSeq; HashCode = Name.hashNames names; TypeCarrier = fun (_ : 'a) -> () }

    /// Make an address from a '/' delimited string.
    let makeFromFullName<'a> fullName =
        Address<'a>.makeFromFullName fullName

    /// Get the names of an address.
    let getNames address =
        address.Names

    /// Change the type of an address.
    let changeType<'a, 'b> (address : 'a Address) =
        { Names = address.Names; HashCode = address.HashCode; TypeCarrier = fun (_ : 'b) -> () }

    /// Get the full name of an address.
    let getFullName address =
        Address<'a>.getFullName address

    /// Get the name of an address.
    let getName address =
        getNames address |> List.last

    /// Get the address's hash code.
    let getHashCode address =
        Address<'a>.hash address

    /// Take the head of an address.
    let head address =
        List.head address.Names
        
    /// Take the tail of an address.
    let tail<'a> address =
        makeFromNames<'a> ^ List.tail address.Names

    /// Take a name of an address.
    let item index address =
        List.item index address.Names

    /// Take an address composed of the name of an address minus a skipped amount of names.
    let skip<'a, 'b> n (address : 'a Address) =
        makeFromNames<'b> ^ List.skip n address.Names

    /// Take an address composed of the given number of names of an address.
    let take<'a, 'b> n (address : 'a Address) =
        makeFromNames<'b> ^ List.take n address.Names

    /// Take the last name of an address.
    let last address =
        List.last address.Names

    /// Take an address composed of all but the last name of an address.
    let allButLast<'a, 'b> (address : 'a Address) =
        makeFromNames<'b> ^ List.allButLast address.Names

    /// Get the length of an address by its names.
    let length address =
        List.length address.Names

    /// Query that an address is devoid of names.
    let isEmpty address =
        List.isEmpty address.Names

[<AutoOpen>]
module AddressOperators =

    /// Convert an address of type 'a to an address of type 'b.
    let atoa<'a, 'b> address =
        Address.changeType<'a, 'b> address

    /// Convert a names list into an address.
    let ltoa<'a> (namesList : _ list) =
        Address.makeFromNames<'a> namesList

    /// Convert a full name into an address.
    let ftoa<'a> fullName =
        Address.makeFromFullName<'a> fullName

    /// Convert a single name into an address.
    let ntoa<'a> name =
        ltoa<'a> [name]

    /// Convert any address to an obj Address.
    let atooa<'a> (address : 'a Address) =
        { Names = address.Names; HashCode = address.HashCode; TypeCarrier = fun (_ : obj) -> () }

    /// Concatenate two addresses of the same type.
    let acat<'a> (address : 'a Address) (address2 : 'a Address) =
        ltoa<'a> (address.Names @ address2.Names)

    /// Concatenate two addresses, taking the type of first address.
    let acatf<'a> (address : 'a Address) (address2 : obj Address) =
        ltoa<'a> (address.Names @ address2.Names)
    
    /// Concatenate two addresses, forcing the type of first address.
    let acatff<'a, 'b> (address : 'a Address) (address2 : 'b Address) =
        acatf address ^ atooa address2

    /// Concatenate two addresses, taking the type of the second address.
    let acats<'a> (address : obj Address) (address2 : 'a Address) =
        ltoa<'a> (address.Names @ address2.Names)
    
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