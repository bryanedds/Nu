// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

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
    /// TODO: check if turning this into a struct could improve performance.
    /// TODO: maybe put this in another place?
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

    let make addressName =
        { Hash = hash addressName
          Name = addressName }

[<AutoOpen>]
module AddressModule =

    /// Specifies the address of an element in a game, or name of an event.
    /// OPTIMIZATION: Comparison is done using a reversed list since the backs of addresses tend to
    /// be much more unique than the fronts.
    /// OPTIMIZATION: In the face of using a PersistentHashMap for simulant storage, I've made the
    /// NameKeys field available for faster look-ups.
    /// OPTIMIZATION: At little cost, I've also added the Hash field for fast keying directly
    /// on addresses.
    type [<CustomEquality; CustomComparison; TypeConverter (typeof<AddressConverter>)>] 't Address =
        { Names : string list
          NamesRev : string list
          NameKeys : NameKey list
          Hash : int
          TypeCarrier : 't -> unit }

        static member internal join (list : string list) =
            String.Join ("/", list)

        static member internal split (str : string) =
            List.ofArray <| str.Split '/'

        /// Make an address from a list of strings.
        static member make list =
            let keys = List.map NameKey.make list
            let hash = List.fold (fun hash (key : NameKey) -> hash ^^^ key.Hash) 0 keys
            { Names = list; NamesRev = List.rev list; NameKeys = keys; Hash = hash; TypeCarrier = fun (_ : 't) -> () }

        /// Convert a string into a list.
        static member stoa (str : string) =
            let list = Address<'t>.split str
            Address<'t>.make list

        /// The empty address.
        static member empty =
            Address<'t>.make []

        interface 't Address IComparable with
            member this.CompareTo that =
                List.compareStrings this.NamesRev that.NamesRev

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? ('t Address) as that -> List.compareStrings this.NamesRev that.NamesRev
                | _ -> failwith "Invalid Address comparison (comparee not of type Address)."

        interface 't Address IEquatable with
            member this.Equals that =
                this.Names = that.Names

        override this.Equals that =
            match that with
            | :? ('t Address) as that -> this.Names = that.Names
            | _ -> false

        override this.GetHashCode () =
            this.Hash
        
        override this.ToString () =
            Address<'t>.join this.Names

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

    /// Convert a string into a list.
    let stoa<'t> str =
        Address<'t>.stoa str

    /// Convert a list into an address.
    let ltoa<'t> list =
        Address<'t>.make list

    /// Convert any address to an obj Address.
    let atooa<'t> (address : 't Address) =
        { Names = address.Names; NamesRev = address.NamesRev; NameKeys = address.NameKeys; Hash = address.Hash; TypeCarrier = fun (_ : obj) -> () }

    /// Concatenate two addresses of the same type.
    let acat (address : 'a Address) (address2 : 'a Address) =
        let list = address.Names @ address2.Names
        Address<'a>.make list

    /// Concatenate two addresses, taking the type of first address.
    let acatf (address : 'a Address) (address2 : obj Address) =
        let list = address.Names @ address2.Names
        Address<'a>.make list
    
    /// Concatenate two addresses, forcing the type of first address.
    let acatff (address : 'a Address) (address2 : 'b Address) =
        acatf address <| atooa address2

    /// Concatenate two addresses, taking the type of the second address.
    let acats (address : obj Address) (address2 : 'a Address) =
        let list = address.Names @ address2.Names
        Address<'a>.make list
    
    /// Concatenate two addresses, forcing the type of second address.
    let acatsf (address : 'a Address) (address2 : 'b Address) =
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
    let changeType<'t, 'u> (address : 't Address) =
        { Names = address.Names; NamesRev = address.NamesRev; NameKeys = address.NameKeys; Hash = address.Hash; TypeCarrier = fun (_ : 'u) -> () }

    /// Take the head of an address.
    let head address =
        List.head address.Names
        
    /// Take the tail of an address.
    let tail address =
        Address.make <| List.tail address.Names

    /// Take a name of an address.
    let at index address =
        List.at index address.Names

    /// Map over an address.
    let map mapper address =
        let list = List.map mapper address.Names
        Address.make list

    /// Filter the names of an address.
    let filter predicate address =
        let list = List.filter predicate address.Names
        Address.make list

    /// Fold over an address.
    let fold folder state address =
        List.fold folder state address.Names

    /// Take an address composed of the names of an address minus a skipped amount of names.
    let skip n address =
        Address.make <| List.skip n address.Names

    /// Take an address composed of the given number of names of an address.
    let take n address =
        Address.make <| List.take n address.Names

    /// Take the last name of an address.
    let last address =
        List.last address.Names

    /// Take an address composed of all but the last name of an address.
    let allButLast address =
        Address.make <| List.allButLast address.Names

    /// Get the length of an address by its names.
    let length address =
        List.length address.Names

    /// Query that an address is devoid of names.
    let isEmpty address =
        List.isEmpty address.Names
