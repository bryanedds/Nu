// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open Xunit
open Prime
open Nu
module AddressTests =

    let [<Fact>] addressConverterConvertToStringWorks () =
        let converter = AddressConverter (typeof<unit Address>)
        let address = stoa<unit> "Some/Thing"
        Assert.Equal ("Some/Thing" :> obj, converter.ConvertToString address)

    let [<Fact>] addressConverterConvertFromStringWorks () =
        let converter = AddressConverter (typeof<unit Address>)
        Assert.Equal (stoa<unit> "Some/Thing" :> obj, converter.ConvertFromString "Some/Thing")