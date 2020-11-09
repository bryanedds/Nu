// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module ReflectionTests =

    type [<ReferenceEquality; NoComparison; CLIMutable>] TestXtended =
        { Xtension : Xtension }

        static member (?) (this : TestXtended, propertyName) =
            Xtension.(?) (this.Xtension, propertyName)

        static member (?<-) (this : TestXtended, propertyName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, propertyName, value)
            { this with Xtension = xtension }

        static member copy (this : TestXtended) =
            { this with Xtension = this.Xtension }

    let [<Fact>] xtensionSerializationViaContainingTypeWorks () =
        let xtd = { Xtension = Xtension.makeMixed () }
        let xtd = xtd?TestProperty <- 5
        let properties = Reflection.writePropertiesFromTarget tautology3 Map.empty xtd
        let xtdRead = { xtd with Xtension = xtd.Xtension } // hacky copy
        let xtd = Reflection.readPropertiesToTarget TestXtended.copy properties xtdRead
        Assert.Equal ((xtd?TestProperty : int), (xtdRead?TestProperty : int))