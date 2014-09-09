// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Configuration
open System.Text
open Prime

[<AutoOpen>]
module AddressModule =

    /// Specifies the address of an element in a game, or name of an event.
    /// OPTIMIZATION: Comparison is done using a reversed list since the backs of addresses tend to
    /// be much more unique than the fronts.
    type [<CustomEquality; CustomComparison>] Address =
        { AddrList : string list
          AddrListRev : string list }

        static member private listToString (list : string list) =
            String.Join ("/", list)

        /// Make an address from a list of strings.
        static member make list =
            { AddrList = list; AddrListRev = List.rev list }

        /// Concatenate two addresses.
        static member (+) (address, address2) =
            let list = address.AddrList @ address2.AddrList
            Address.make list

        interface Address IComparable with
            member this.CompareTo that =
                List.compareStrings this.AddrListRev that.AddrListRev

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Address as that -> List.compareStrings this.AddrListRev that.AddrListRev
                | _ -> failwith "Invalid Address comparison (comparee not of type Address)."

        interface Address IEquatable with
            member this.Equals that =
                this.AddrList = that.AddrList

        override this.Equals that =
            match that with
            | :? Address as that -> this.AddrList = that.AddrList
            | _ -> false

        override this.GetHashCode () =
            let mutable result = 0
            for name in this.AddrList do
                result <- result ^^^ (name.GetHashCode ())
            result
        
        override this.ToString () =
            String.Join ("/", this.AddrList)

    /// Make an address from a list of strings.
    let make =
        Address.make

    /// Make an address by splitting a string on the '/' character.
    let addr (str : string) : Address =
        let list = List.ofArray <| str.Split '/'
        make list

    /// Concatenate a list of strings to the front of an address.
    let listaddr list address =
        make <| list @ address.AddrList

    /// Concatenate a list of strings to the back of an address.
    let addrlist address list =
        make <| address.AddrList @ list

    /// Surround an address with two lists of strings.
    let listaddrlist list address list2 =
        make <| list @ address.AddrList @ list2

[<RequireQualifiedAccess>]
module Address =

    /// The empty address.
    let empty =
        make []

    /// Take the head of an address.
    let head address =
        List.head address.AddrList
        
    /// Take the tail of an address.
    let tail address =
        make <| List.tail address.AddrList

    /// Take an element of an address.
    let at index address =
        List.at index address.AddrList

    /// Map over an address.
    let map mapper address =
        let addrList = List.map mapper address.AddrList
        make addrList

    /// Filter the elements of an address.
    let filter predicate address =
        let addrList = List.filter predicate address.AddrList
        make addrList

    /// Fold over an address.
    let fold folder state address =
        List.fold folder state address.AddrList

    /// Take an address composed of the elements of an address minus a skipped amount of elements.
    let skip n address =
        make <| List.skip n address.AddrList

    /// Take an address composed of the given number of elements of an address.
    let take n address =
        make <| List.take n address.AddrList

    /// Take the last element of an address.
    let last address =
        List.last address.AddrList

    /// Take an address composed of all but the last element of an address.
    let allButLast address =
        make <| List.allButLast address.AddrList

    /// Get the length of an address by its elements.
    let length address =
        List.length address.AddrList

    /// Query that an address is devoid of elements.
    let isEmpty address =
        List.isEmpty address.AddrList