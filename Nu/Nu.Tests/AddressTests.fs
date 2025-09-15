// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module AddressTests =

    let [<Test>] ``Address.relate overflow works properly.`` () =
        let address = stoa "A/B/~"
        let address2 = stoa "A/B/~/D/E"
        let relative = Address.relate address address2
        Assert.Equal (stoa "~/D/E", relative)

    let [<Test>] ``Address.relate underflow works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "A/B/C"
        let relative = Address.relate address address2
        Assert.Equal (stoa "^/^", relative)

    let [<Test>] ``Address.relate even and same works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "A/B/C/D/E"
        let relative = Address.relate address address2
        Assert.Equal (stoa "~", relative)

    let [<Test>] ``Address.relate even but different works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "A/B/C/D/F"
        let relative = Address.relate address address2
        Assert.Equal (stoa "^/F", relative)

    let [<Test>] ``Address.relate unrelated works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "B/C/D/F/A"
        let relative = Address.relate address address2
        Assert.Equal (stoa "B/C/D/F/A", relative)

    let [<Test>] ``Address.resolve relative simple ancestor works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "^/^"
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "A/B/C", resolved)

    let [<Test>] ``Address.resolve relative simple successor properly.`` () =
        let address = stoa "A/B/C"
        let address2 = stoa "~/D/E"
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "A/B/C/D/E", resolved)

    let [<Test>] ``Address.resolve relative complex works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "^/~/^/D/^/X"
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "A/B/C/X", resolved)

    let [<Test>] ``Address.resolve empty works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = Address.empty
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "A/B/C/D/E", resolved)

    let [<Test>] ``Address.resolve absolute works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "B/C/D/F/A"
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "B/C/D/F/A", resolved)