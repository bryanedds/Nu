// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Tests
open System
open System.IO
open Xunit
open Prime
open Nu
module ReflectionTests =

    type [<CLIMutable; NoEquality; NoComparison>] TestXtended =
        { Xtension : Xtension }

        static member (?) (this : TestXtended, propertyName) =
            Xtension.(?) (this.Xtension, propertyName)

        static member (?<-) (this : TestXtended, propertyName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, propertyName, value)
            { this with Xtension = xtension }

    let [<Fact>] xtensionSerializationViaContainingTypeWorks () =
        let xtd = { Xtension = Xtension.mixed }
        let xtd = xtd?TestProperty <- 5
        let properties = Reflection.writeMemberValuesFromTarget tautology3 Map.empty xtd
        let xtdRead = { xtd with Xtension = xtd.Xtension } // hacky copy
        Reflection.readMemberValuesToTarget properties xtdRead
        Assert.Equal((xtd?TestProperty : int), (xtdRead?TestProperty : int))