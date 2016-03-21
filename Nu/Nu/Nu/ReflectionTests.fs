// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Tests
open System
open System.IO
open System.Xml
open Xunit
open Prime
open Nu
module ReflectionTests =

    type [<CLIMutable; NoEquality; NoComparison>] TestXtended =
        { Xtension : Xtension }

        static member (?) (this : TestXtended, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : TestXtended, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    let [<Fact>] xtensionSerializationViaContainingTypeWorks () =
        let xtd = { Xtension = Xtension.mixed }
        let xtd = xtd?TestField <- 5
        let fields = Reflection.writeMemberValuesFromTarget tautology3 Map.empty xtd
        let xtdRead = { xtd with Xtension = xtd.Xtension } // hacky copy
        Reflection.readMemberValuesToTarget fields xtdRead
        Assert.Equal((xtd?TestField : int), (xtdRead?TestField : int))