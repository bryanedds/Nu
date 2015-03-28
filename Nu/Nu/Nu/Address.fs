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

[<AutoOpen>]
module NameKeyModule =

    /// A name key for optimized look-up in hashing containers.
    type [<CustomEquality; NoComparison>] NameKey =
        { Hash : int // OPTIMIZATION: hash is most frequently accessed, so comes first
          Name : string }

        interface NameKey IEquatable with
            member this.Equals that =
                this.Name = that.Name

        override this.Equals that =
            match that with
            | :? NameKey as that -> this.Name = that.Name
            | _ -> false

        override this.GetHashCode () =
            this.Hash

[<RequireQualifiedAccess>]
module NameKey =

    /// Make a name key from a single address name string.
    let make addressName =
        { Hash = hash addressName
          Name = addressName }

[<AutoOpen>]
module AddressModule =

    /// Specifies the address of an element in a game, or name of an event.
    /// OPTIMIZATION: Comparison is done using the full string of names for speed.
    /// OPTIMIZATION: In the face of using a PersistentHashMap for simulant storage, I've made the
    /// NameKeys field available for faster look-ups.
    /// OPTIMIZATION: At little cost, I've also added the Hash field for fast keying directly
    /// on addresses.
    type [<CustomEquality; CustomComparison; TypeConverter (typeof<AddressConverter>)>] 'a Address =
        { NamesStr : string
          NameKeys : NameKey list
          Hash : int
          TypeCarrier : 'a -> unit }

        static member internal join (seq : string seq) =
            String.Join ("/", seq)

        static member internal split (str : string) =
            List.ofArray <| str.Split '/'

        /// Make an address from a list of names.
        static member makeFromList list =
            let str = Address<'a>.join list
            let keys = List.map NameKey.make list
            { NamesStr = str; NameKeys = keys; Hash = hash str; TypeCarrier = fun (_ : 'a) -> () }

        /// Make an address from a list of names.
        static member makeFromStrAndKeys str keys =
            { NamesStr = str; NameKeys = keys; Hash = hash str; TypeCarrier = fun (_ : 'a) -> () }

        /// Make an address from a list of names.
        static member makeFromKeys keys =
            let str = keys |> Seq.map (fun key -> key.Name) |> Address<'a>.join
            Address<'a>.makeFromStrAndKeys str keys

        static member makeFromString str =
            let list = Address<'a>.split str
            let keys = List.map NameKey.make list
            { NamesStr = str; NameKeys = keys; Hash = hash str; TypeCarrier = fun (_ : 'a) -> () }

        /// Convert a string into a list.
        static member stoa (str : string) =
            Address<'a>.makeFromString str

        /// The empty address.
        static member empty =
            { NamesStr = String.Empty; NameKeys = []; Hash = 0; TypeCarrier = fun (_ : 'a) -> () }

        interface 'a Address IComparable with
            member this.CompareTo that =
                String.Compare (this.NamesStr, that.NamesStr)

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? ('a Address) as that -> String.Compare (this.NamesStr, that.NamesStr)
                | _ -> failwith "Invalid Address comparison (comparee not of type Address)."

        interface 'a Address IEquatable with
            member this.Equals that =
                this.NamesStr = that.NamesStr

        override this.Equals that =
            match that with
            | :? ('a Address) as that -> this.NamesStr = that.NamesStr
            | _ -> false

        override this.GetHashCode () =
            this.Hash
        
        override this.ToString () =
            this.NamesStr

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

    /// Convert a string into an address.
    let stoa<'a> str =
        Address<'a>.stoa str

    /// Convert a name key list into an address.
    let ktoa<'a> keys =
        Address<'a>.makeFromKeys keys

    /// Convert a list into an address.
    let ltoa<'a> list =
        Address<'a>.makeFromList list

    /// Convert a single name into an address.
    let ntoa<'a> name =
        ltoa<'a> [name]

    /// Convert any address to an obj Address.
    let atooa<'a> (address : 'a Address) =
        { NamesStr = address.NamesStr; NameKeys = address.NameKeys; Hash = address.Hash; TypeCarrier = fun (_ : obj) -> () }

    /// Concatenate two addresses of the same type.
    let acat<'a> (address : 'a Address) (address2 : 'a Address) =
        Address<'a>.makeFromStrAndKeys
            (Address<'a>.join [address.NamesStr; address2.NamesStr])
            (address.NameKeys @ address2.NameKeys)

    /// Concatenate two addresses, taking the type of first address.
    let acatf<'a> (address : 'a Address) (address2 : obj Address) =
        Address<'a>.makeFromStrAndKeys
            (Address<'a>.join [address.NamesStr; address2.NamesStr])
            (address.NameKeys @ address2.NameKeys)
    
    /// Concatenate two addresses, forcing the type of first address.
    let acatff<'a, 'b> (address : 'a Address) (address2 : 'b Address) =
        acatf address <| atooa address2

    /// Concatenate two addresses, taking the type of the second address.
    let acats<'a> (address : obj Address) (address2 : 'a Address) =
        Address<'a>.makeFromStrAndKeys
            (Address<'a>.join [address.NamesStr; address2.NamesStr])
            (address.NameKeys @ address2.NameKeys)
    
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
        { NamesStr = address.NamesStr; NameKeys = address.NameKeys; Hash = address.Hash; TypeCarrier = fun (_ : 'b) -> () }

    /// Take the head of an address.
    let head address =
        List.head address.NameKeys
        
    /// Take the tail of an address.
    let tail<'a> address =
        Address<'a>.makeFromKeys <| List.tail address.NameKeys

    /// Take a name key of an address.
    let at index address =
        List.at index address.NameKeys

    /// Take an address composed of the names of an address minus a skipped amount of names.
    let skip<'a, 'b> n (address : 'a Address) =
        Address<'b>.makeFromKeys <| List.skip n address.NameKeys

    /// Take an address composed of the given number of names of an address.
    let take<'a, 'b> n (address : 'a Address) =
        Address<'b>.makeFromKeys <| List.take n address.NameKeys

    /// Take the last name of an address.
    let last address =
        List.last address.NameKeys

    /// Take an address composed of all but the last name of an address.
    let allButLast<'a, 'b> (address : 'a Address) =
        Address<'b>.makeFromKeys <| List.allButLast address.NameKeys

    /// Get the length of an address by its names.
    let length address =
        List.length address.NameKeys

    /// Query that an address is devoid of names.
    let isEmpty address =
        List.isEmpty address.NameKeys