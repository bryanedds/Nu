// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Configuration
open System.Text
open Prime

[<AutoOpen>]
module NuCoreModule =

    /// Specifies the screen-clearing routine.
    type ScreenClear =
        | NoClear
        | ColorClear of byte * byte * byte

    /// Specifies whether the engine is running or exiting.
    type Liveness =
        | Exiting
        | Running

    /// Specifies the address of an element in a game.
    /// OPTIMIZATION: Comparison is done using a reversed list since the backs of addresses tend to
    /// be much more unique than the fronts.
    type [<CustomEquality; CustomComparison>] Address =
        { AddrList : string list
          AddrListRev : string list }

        static member private listToString (list : string list) =
            String.Join ("/", list)

        static member make list =
            { AddrList = list; AddrListRev = List.rev list }

        static member (+) (address, address2) =
            let list = address.AddrList @ address2.AddrList
            Address.make list

        static member private innerCompareTo this that =
            List.compareStrings this.AddrListRev that.AddrListRev
        
        static member private innerEquals this that =
            this.AddrList = that.AddrList

        interface Address IComparable with
            member this.CompareTo that =
                Address.innerCompareTo this that

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Address as that -> Address.innerCompareTo this that
                | _ -> failwith "Invalid Address comparison (comparee not of type Address)."

        interface Address IEquatable with
            member this.Equals that =
                Address.innerEquals this that

        override this.Equals that =
            match that with
            | :? Address as that -> Address.innerEquals this that
            | _ -> false

        override this.GetHashCode () =
            let mutable hash = 0
            for name in this.AddrList do
                hash <- hash ^^^ name.GetHashCode ()
            hash
        
        override this.ToString () =
            String.Join ("/", this.AddrList)

    let make =
        Address.make

    let addr (str : string) : Address =
        let list = List.ofArray <| str.Split '/'
        make list

    let straddr str address =
        addr str + address

    let addrstr address str =
        let list = address.AddrList @ [str]
        make list

    let straddrstr str address str2 =
        addr str + address + addr str2

    let listaddr list address =
        make <| list @ address.AddrList

    let addrlist address list =
        make <| address.AddrList @ list

    let listaddrlist list address list2 =
        make <| list @ address.AddrList @ list2

[<RequireQualifiedAccess>]
module NuCore =

    /// The invalid Id.
    let InvalidId = Guid.Empty
    
    /// Make a Nu Id.
    let makeId = Guid.NewGuid

    let getResolutionOrDefault isX defaultResolution =
        let resolution = ref 0
        let appSetting = ConfigurationManager.AppSettings.["Resolution" + if isX then "X" else "Y"]
        if not <| Int32.TryParse (appSetting, resolution) then resolution := defaultResolution
        !resolution

[<RequireQualifiedAccess>]
module Address =

    let empty =
        make []

    let head address =
        List.head address.AddrList
        
    let tail address =
        make <| List.tail address.AddrList

    let at index address =
        List.at index address.AddrList

    let map mapper address =
        let addrList = List.map mapper address.AddrList
        make addrList

    let filter predicate address =
        let addrList = List.filter predicate address.AddrList
        make addrList

    let fold folder state address =
        List.fold folder state address.AddrList

    let skip n address =
        make <| List.skip n address.AddrList

    let take n address =
        make <| List.take n address.AddrList

    let last address =
        List.last address.AddrList

    let allButLast address =
        make <| List.allButLast address.AddrList

    let length address =
        List.length address.AddrList

    let isEmpty address =
        List.isEmpty address.AddrList