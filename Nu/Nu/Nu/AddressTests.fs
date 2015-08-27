// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module AddressTests =

    let [<Fact>] addressConverterConvertToStringWorks () =
        let converter = AddressConverter (typeof<unit Address>)
        let address = nstoa<unit> "Some/Thing"
        Assert.Equal ("Some/Thing" :> obj, converter.ConvertToString address)

    let [<Fact>] addressConverterConvertFromStringWorks () =
        let converter = AddressConverter (typeof<unit Address>)
        Assert.Equal (nstoa<unit> "Some/Thing" :> obj, converter.ConvertFromString "Some/Thing")