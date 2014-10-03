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

        static member internal join (list : string list) =
            String.Join ("/", list)

        static member internal split (str : string) =
            List.ofArray <| str.Split '/'

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
                result <- result ^^^ hash name
            result
        
        override this.ToString () =
            Address.join this.AddrList

    /// Make an address by splitting a string on the '/' character.
    let ( !* ) (str : string) : Address =
        let list = Address.split str
        Address.make list

    /// Split a string on the '/' character and then concatenate it to the front of an address.
    let ( *@ ) (str : string) address =
        Address.make <| Address.split str @ address.AddrList

    /// Split a string on the '/' character and then concatenate it to the back of an address.
    let ( @* ) address (str : string) =
        Address.make <| address.AddrList @ Address.split str

    /// Split two strings on the '/' character and then surround an address with them.
    let ( *@* ) (str : string) address (str2 : string) =
        Address.make <| Address.split str @ address.AddrList @ Address.split str2

    /// Make an address from a list of strings.
    let (!+) list =
        Address.make list

    /// Concatenate a list of strings to the front of an address.
    let (+@) list address =
        Address.make <| list @ address.AddrList

    /// Concatenate a list of strings to the back of an address.
    let (@+) address list =
        Address.make <| address.AddrList @ list

    /// Surround an address with two lists of strings.
    let (+@+) list address list2 =
        Address.make <| list @ address.AddrList @ list2

[<RequireQualifiedAccess>]
module Address =

    /// The empty address.
    let empty =
        Address.make []

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