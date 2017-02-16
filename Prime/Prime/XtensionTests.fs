// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime.Tests
open System
open Xunit
open Prime

type [<CLIMutable; NoEquality; NoComparison>] TestXtended =
    { Xtension : Xtension }

    static member (?) (this : TestXtended, propertyName) =
        Xtension.(?) (this.Xtension, propertyName)

    static member (?<-) (this : TestXtended, propertyName, value) =
        let xtension = Xtension.(?<-) (this.Xtension, propertyName, value)
        { this with Xtension = xtension }

module XtensionTests =

    let [<Fact>] canAddProperty () =
        let xtn = Xtension.empty
        let xtn = xtn?TestProperty <- 5
        let propertyValue = xtn?TestProperty
        Assert.Equal (5, propertyValue)

    let [<Fact>] cantAddPropertyWhenSealed () =
        let xtn = Xtension.safe
        Assert.Throws<Exception> (fun () -> (xtn?TestProperty <- 0) |> ignore)

    let [<Fact>] cantAccessNonexistentProperty () =
        let xtn = Xtension.mixed
        let xtn = xtn?TestProperty <- 5
        Assert.Throws<Exception> (fun () -> xtn?TetProperty |> ignore)

    let [<Fact>] missingPropertyReturnsDefault () =
        let xtn = Xtension.empty
        let xtn = xtn?TestProperty <- 0
        let propertyValue = xtn?MissingProperty
        Assert.Equal (0, propertyValue)

    let [<Fact>] canAddPropertyViaContainingType () =
        let xtd = { Xtension = Xtension.empty }
        let xtd = xtd?TestProperty <- 5
        let propertyValue = xtd?TestProperty
        Assert.Equal (5, propertyValue)