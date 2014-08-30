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
    /// Comparison disabled due to performance issues when Address is using in a Map.
    type [<StructuralEquality; NoComparison>] Address =
        { AddrList : string list
          AddrStr : string }

        static member private listToString (list : string list) =
            String.Join ("/", list)

        static member makeAddress list =
            { AddrList = list; AddrStr = Address.listToString list }

        static member (+) (address, address2) =
            let list = address.AddrList @ address2.AddrList
            Address.makeAddress list

        static member private innerCompareTo this that =
            String.Compare (this.AddrStr, that.AddrStr)
        
        override this.ToString () =
            this.AddrStr

    let makeAddress =
        Address.makeAddress

    let addr (str : string) : Address =
        let list = List.ofArray <| str.Split '/'
        makeAddress list

    let straddr str address =
        addr str + address

    let addrstr address str =
        let list = address.AddrList @ [str]
        makeAddress list

    let straddrstr str address str2 =
        addr str + address + addr str2

    let listaddr list address =
        makeAddress <| list @ address.AddrList

    let addrlist address list =
        makeAddress <| address.AddrList @ list

    let listaddrlist list address list2 =
        makeAddress <| list @ address.AddrList @ list2

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
        makeAddress []

    let head address =
        List.head address.AddrList
        
    let tail address =
        makeAddress <| List.tail address.AddrList

    let at index address =
        List.at index address.AddrList

    let map mapper address =
        let addrList = List.map mapper address.AddrList
        makeAddress addrList

    let filter predicate address =
        let addrList = List.filter predicate address.AddrList
        makeAddress addrList

    let fold folder state address =
        List.fold folder state address.AddrList

    let skip n address =
        makeAddress <| List.skip n address.AddrList

    let take n address =
        makeAddress <| List.take n address.AddrList

    let last address =
        List.last address.AddrList

    let allButLast address =
        makeAddress <| List.allButLast address.AddrList

    let length address =
        List.length address.AddrList

    let isEmpty address =
        List.isEmpty address.AddrList