// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Configuration
open System.Text
open Prime
open Nu

[<AutoOpen>]
module NameKeyModule =

    /// A name key for optimized look-up in hashing containers.
    /// TODO: check if turning this into a struct could improve performance.
    /// TODO: maybe put this in another place?
    type [<CustomEquality; NoComparison>] NameKey =
        { NKHash : int // OPTIMIZATION: hash is most frequently accessed, so comes first
          NKName : string }

        interface NameKey IEquatable with
            member this.Equals that =
                this.NKName = that.NKName

        override this.Equals that =
            match that with
            | :? NameKey as that -> this.NKName = that.NKName
            | _ -> false

        override this.GetHashCode () =
            this.NKHash

[<RequireQualifiedAccess>]
module NameKey =

    let make addressName =
        { NKHash = hash addressName
          NKName = addressName }

[<AutoOpen>]
module AddressModule =

    /// Specifies the address of an element in a game, or name of an event.
    /// OPTIMIZATION: Comparison is done using a reversed list since the backs of addresses tend to
    /// be much more unique than the fronts.
    /// OPTIMIZATION: In the face of using a PersistentHashMap for simulant storage, I've made the
    /// AddrNameKeys field available for faster look-ups.
    /// OPTIMIZATION: At little cost, I've also added the AddrHash field for fast keying directly
    /// on addresses.
    type [<CustomEquality; CustomComparison>] 't Address =
        { AddrList : string list
          AddrListRev : string list
          AddrNameKeys : NameKey list
          AddrHash : int
          AddrTypeCarrier : 't -> unit }

        static member internal join (list : string list) =
            String.Join ("/", list)

        static member internal split (str : string) =
            List.ofArray <| str.Split '/'

        /// Make an address from a list of strings.
        static member make list =
            let keys = List.map NameKey.make list
            let hash = List.fold (fun hash key -> hash ^^^ key.NKHash) 0 keys
            { AddrList = list; AddrListRev = List.rev list; AddrNameKeys = keys; AddrHash = hash; AddrTypeCarrier = fun (_ : 't) -> () }

        interface 't Address IComparable with
            member this.CompareTo that =
                List.compareStrings this.AddrListRev that.AddrListRev

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? ('t Address) as that -> List.compareStrings this.AddrListRev that.AddrListRev
                | _ -> failwith "Invalid Address comparison (comparee not of type Address)."

        interface 't Address IEquatable with
            member this.Equals that =
                this.AddrList = that.AddrList

        override this.Equals that =
            match that with
            | :? ('t Address) as that -> this.AddrList = that.AddrList
            | _ -> false

        override this.GetHashCode () =
            this.AddrHash
        
        override this.ToString () =
            Address<'t>.join this.AddrList

    /// Concatenate two addresses of the same type.
    let acat (address : 'a Address) (address2 : 'a Address) =
        let list = address.AddrList @ address2.AddrList
        Address<'a>.make list

    /// Concatenate two addresses, taking the type of first address.
    let acatf (address : 'a Address) (address2 : obj Address) =
        let list = address.AddrList @ address2.AddrList
        Address<'a>.make list

    /// Concatenate two addresses, taking the type of the second address.
    let acats (address : obj Address) (address2 : 'b Address) =
        let list = address.AddrList @ address2.AddrList
        Address<'b>.make list
    
    /// Concatenate two addresses of the same type.
    let (-|-) = acat

    /// Concatenate two addresses, taking the type of first address.
    let (-<-) = acatf

    /// Concatenate two addresses, taking the type of the second address.
    let (->-) = acats

    /// Convert a string into a list.
    let stoa<'t> (str : string) =
        let list = Address<'t>.split str
        Address<'t>.make list

    /// Convert a list into a list.
    let ltoa<'t> list =
        Address<'t>.make list

    /// Convert any address to an obj Address.
    let atoo address =
        Address<obj>.make address.AddrList

[<RequireQualifiedAccess>]
module Address =

    /// The empty address.
    let empty<'t> =
        Address<'t>.make []

    /// Take the head of an address.
    let head address =
        List.head address.AddrList
        
    /// Take the tail of an address.
    let tail address =
        Address.make <| List.tail address.AddrList

    /// Take an element of an address.
    let at index address =
        List.at index address.AddrList

    /// Map over an address.
    let map mapper address =
        let list = List.map mapper address.AddrList
        Address.make list

    /// Filter the elements of an address.
    let filter predicate address =
        let list = List.filter predicate address.AddrList
        Address.make list

    /// Fold over an address.
    let fold folder state address =
        List.fold folder state address.AddrList

    /// Take an address composed of the elements of an address minus a skipped amount of elements.
    let skip n address =
        Address.make <| List.skip n address.AddrList

    /// Take an address composed of the given number of elements of an address.
    let take n address =
        Address.make <| List.take n address.AddrList

    /// Take the last element of an address.
    let last address =
        List.last address.AddrList

    /// Take an address composed of all but the last element of an address.
    let allButLast address =
        Address.make <| List.allButLast address.AddrList

    /// Get the length of an address by its elements.
    let length address =
        List.length address.AddrList

    /// Query that an address is devoid of elements.
    let isEmpty address =
        List.isEmpty address.AddrList