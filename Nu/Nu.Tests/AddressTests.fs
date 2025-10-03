// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module AddressTests =

    let [<Test>] ``Relate overflow works properly.`` () =
        let address = stoa "A/B/C"
        let address2 = stoa "A/B/C/D/E"
        let relative = Address.relate address address2
        Assert.Equal (stoa "~/D/E", relative)

    let [<Test>] ``Relate underflow works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "A/B/C"
        let relative = Address.relate address address2
        Assert.Equal (stoa "^/^", relative)

    let [<Test>] ``Relate even and same works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "A/B/C/D/E"
        let relative = Address.relate address address2
        Assert.Equal (stoa "~", relative)

    let [<Test>] ``Relate even but different works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "A/B/C/D/F"
        let relative = Address.relate address address2
        Assert.Equal (stoa "^/F", relative)

    let [<Test>] ``Relate unrelated works properly.`` () =
        let address = stoa "A/B/C/D/E"
        let address2 = stoa "B/C/D/F/A"
        let relative = Address.relate address address2
        Assert.Equal (stoa "B/C/D/F/A", relative)

    let [<Test>] ``Resolve relative simple ancestor works properly.`` () =
        let address = stoa "^/^"
        let address2 = stoa "A/B/C/D/E"
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "A/B/C", resolved)

    let [<Test>] ``Resolve relative simple successor properly.`` () =
        let address = stoa "~/D/E"
        let address2 = stoa "A/B/C"
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "A/B/C/D/E", resolved)

    let [<Test>] ``Resolve relative complex works properly.`` () =
        let address = stoa "^/~/^/D/^/X"
        let address2 = stoa "A/B/C/D/E"
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "A/B/C/X", resolved)

    let [<Test>] ``Resolve empty works properly.`` () =
        let address = Address.empty
        let address2 = stoa "A/B/C/D/E"
        let resolved = Address.resolve address address2
        Assert.Equal (Address.empty, resolved)

    let [<Test>] ``Resolve absolute works properly.`` () =
        let address = stoa "B/C/D/F/A"
        let address2 = stoa "A/B/C/D/E"
        let resolved = Address.resolve address address2
        Assert.Equal (stoa "B/C/D/F/A", resolved)

    let [<Test>] ``Relate and Address.resolve work with backslashes.`` () =
        let address = stoa "A/B/C/D"
        let address2 = stoa "A/B\\C/D"
        let relative = Address.relate address address2
        Assert.Equal (stoa "^/^/^/B\\C/D", relative)
        Assert.Equal (address2, Address.resolve relative address)

    let [<TestCase ("^", "", ""); // NOTE: an empty string means an address of one empty name.
          TestCase ("^", "[]", "[]"); // NOTE: [] is the empty address.
          TestCase ("^", "Name", "Name");
          TestCase ("^", "^", "^/^");
          TestCase ("^", "~", "^");
          TestCase ("^", "~/Name", "^/Name");
          TestCase ("^", "~/Name/^/^", "^/^");
          TestCase ("^", "^/Name", "^/^/Name");
          TestCase ("^", "/^", "[]");
          TestCase ("^", "/~", "");
          TestCase ("^", "/Name", "/Name");
          TestCase ("^", "/Name/^", "");
          TestCase ("^", "/Name/~", "/Name");
          TestCase ("^", "~/^", "^/^");
          TestCase ("^", "^/^", "^/^/^");
          TestCase ("^/", "", "");
          TestCase ("^/", "[]", "[]");
          TestCase ("^/", "Name", "Name");
          TestCase ("^/", "^", "^");
          TestCase ("^/", "~", "^/");
          TestCase ("^/", "~/Name", "^//Name");
          TestCase ("^/", "~/Name/^/^", "^");
          TestCase ("^/", "^/Name", "^/Name");
          TestCase ("^/", "/^", "[]");
          TestCase ("^/", "/~", "");
          TestCase ("^/", "/Name", "/Name");
          TestCase ("^/", "/Name/^", "");
          TestCase ("^/", "/Name/~", "/Name");
          TestCase ("^/", "~/^", "^");
          TestCase ("^/", "^/^", "^/^");
          TestCase ("~", "", "");
          TestCase ("~", "[]", "[]");
          TestCase ("~", "Name", "Name");
          TestCase ("~", "^", "^");
          TestCase ("~", "~", "~");
          TestCase ("~", "~/Name", "~/Name");
          TestCase ("~", "~/Name/^/^", "^");
          TestCase ("~", "^/Name", "^/Name");
          TestCase ("~", "/^", "[]");
          TestCase ("~", "/~", "");
          TestCase ("~", "/Name", "/Name");
          TestCase ("~", "/Name/^", "");
          TestCase ("~", "/Name/~", "/Name");
          TestCase ("~", "~/^", "^");
          TestCase ("~", "^/^", "^/^");
          TestCase ("~/", "", "");
          TestCase ("~/", "[]", "[]");
          TestCase ("~/", "Name", "Name");
          TestCase ("~/", "^", "~");
          TestCase ("~/", "~", "~/");
          TestCase ("~/", "~/Name", "~//Name");
          TestCase ("~/", "~/Name/^/^", "~");
          TestCase ("~/", "^/Name", "~/Name");
          TestCase ("~/", "/^", "[]");
          TestCase ("~/", "/~", "");
          TestCase ("~/", "/Name", "/Name");
          TestCase ("~/", "/Name/^", "");
          TestCase ("~/", "/Name/~", "/Name");
          TestCase ("~/", "~/^", "~");
          TestCase ("~/", "^/^", "^");
          TestCase ("/^", "", "");
          TestCase ("/^", "[]", "[]");
          TestCase ("/^", "Name", "Name");
          TestCase ("/^", "^", "[]");
          TestCase ("/^", "~", "[]");
          TestCase ("/^", "~/Name", "Name");
          TestCase ("/^", "~/Name/^/^", "[]");
          TestCase ("/^", "^/Name", "Name");
          TestCase ("/^", "/^", "[]");
          TestCase ("/^", "/~", "");
          TestCase ("/^", "/Name", "/Name");
          TestCase ("/^", "/Name/^", "");
          TestCase ("/^", "/Name/~", "/Name");
          TestCase ("/^", "~/^", "[]");
          TestCase ("/^", "^/^", "[]");
          TestCase ("/^/", "", "");
          TestCase ("/^/", "[]", "[]");
          TestCase ("/^/", "Name", "Name");
          TestCase ("/^/", "^", "[]");
          TestCase ("/^/", "~", "");
          TestCase ("/^/", "~/Name", "/Name");
          TestCase ("/^/", "~/Name/^/^", "[]");
          TestCase ("/^/", "^/Name", "Name");
          TestCase ("/^/", "/^", "[]");
          TestCase ("/^/", "/~", "");
          TestCase ("/^/", "/Name", "/Name");
          TestCase ("/^/", "/Name/^", "");
          TestCase ("/^/", "/Name/~", "/Name");
          TestCase ("/^/", "~/^", "[]");
          TestCase ("/^/", "^/^", "[]");
          TestCase ("/~", "", "");
          TestCase ("/~", "[]", "[]");
          TestCase ("/~", "Name", "Name");
          TestCase ("/~", "^", "[]");
          TestCase ("/~", "~", "");
          TestCase ("/~", "~/Name", "/Name");
          TestCase ("/~", "~/Name/^/^", "[]");
          TestCase ("/~", "^/Name", "Name");
          TestCase ("/~", "/^", "[]");
          TestCase ("/~", "/~", "");
          TestCase ("/~", "/Name", "/Name");
          TestCase ("/~", "/Name/^", "");
          TestCase ("/~", "/Name/~", "/Name");
          TestCase ("/~", "~/^", "[]");
          TestCase ("/~", "^/^", "[]");
          TestCase ("/~/", "", "");
          TestCase ("/~/", "[]", "[]");
          TestCase ("/~/", "Name", "Name");
          TestCase ("/~/", "^", "");
          TestCase ("/~/", "~", "/");
          TestCase ("/~/", "~/Name", "//Name");
          TestCase ("/~/", "~/Name/^/^", "");
          TestCase ("/~/", "^/Name", "/Name");
          TestCase ("/~/", "/^", "[]");
          TestCase ("/~/", "/~", "");
          TestCase ("/~/", "/Name", "/Name");
          TestCase ("/~/", "/Name/^", "");
          TestCase ("/~/", "/Name/~", "/Name");
          TestCase ("/~/", "~/^", "");
          TestCase ("/~/", "^/^", "[]");>]
        ``Resolve works with relative paths.`` (addressStr, relationStr, relativeStr) =
        let address = Address.stoa addressStr
        let relation = Address.stoa relationStr
        let relative = Address.stoa relativeStr
        Assert.Equal (relative, Address.resolve relation address)

    let [<TestCase ("*/*", "* cannot appear together with another *");
          TestCase (".../a", "... cannot be before the last name");
          TestCase ("*/...", "... cannot appear together with another *");
          TestCase ("a/[]", "[] cannot be an address name")>]
        ``Resolve throws for invalid addresses.`` (addressStr, exnStr) =
        let address = Address.stoa addressStr
        Assert.Equal ("address",
            Assert.Throws<ArgumentException>(
                fun () -> Address.resolve (Address.stoa "~/Name") address |> ignore
                , exnStr).ParamName)
        Assert.Equal ("relation",
            Assert.Throws<ArgumentException>(
                fun () -> Address.resolve address (Address.stoa "Name") |> ignore
                , exnStr).ParamName)
        // An absolute relation should ignore the address altogether.
        Assert.Equal (Address.stoa "Name", Address.resolve (Address.stoa "Name") address)

    let [<TestCase ("Name/^", "Name/~", "Name");
          TestCase ("Name/^/Name/~/~", "Name/^/^/^", "[]");
          TestCase ("Name/^/Name", "Name/~", "~");
          TestCase ("Name/^/Name", "Name/~/N", "~/N");
          TestCase ("Name/^/Name/~/~", "Name/~/~/Name/^", "~");
          TestCase ("A/B/C/^/D/~/E", "A/F", "^/^/^/F");>]
        ``Relate works with relational symbols.`` (sourceStr, destinationStr, relativeStr) =
        let address = stoa sourceStr
        let address2 = stoa destinationStr
        let relative = Address.relate address address2
        Assert.Equal (stoa relativeStr, relative)

    let [<TestCase "^";
          TestCase "~";
          TestCase "^/Name";
          TestCase "~/Name">]
        ``Relate cannot work with relative addresses.`` (addressStr) =

        // check that source argument causes exception
        let address = Address.stoa addressStr
        let address2 = Address.stoa "Name"
        Assert.Throws<ArgumentException> (fun () -> Address.relate address address2 |> ignore)
        |> fun exn -> Assert.Equal ("source", exn.ParamName)

        // check that destination argument causes exception
        Assert.Throws<ArgumentException> (fun () -> Address.relate address2 address |> ignore)
        |> fun exn -> Assert.Equal ("destination", exn.ParamName)