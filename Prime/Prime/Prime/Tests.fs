// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open Xunit
module Tests =

    let [<Fact>] lunComparisonTest () =
        let abc = Lun.make "abc" :> IComparable
        let cba = Lun.make "cba" :> IComparable
        Assert.True (abc.CompareTo cba = -1)
        Assert.True (abc.CompareTo abc = 0)
        Assert.True (cba.CompareTo abc = 1)

    let [<Fact>] fastLunComparisonTest () =
        let abc = Lun.makeFast "abc" :> IComparable
        let cba = Lun.makeFast "cba" :> IComparable
        Assert.True (abc.CompareTo cba = -1)
        Assert.True (abc.CompareTo abc = 0)
        Assert.True (cba.CompareTo abc = 1)

    let [<Fact>] lunHasFastComparisonTest () =
        Assert.True (Lun.make "abc").LunOptNums.IsSome
        Assert.True (Lun.makeFast "abc").LunOptNums.IsNone
        Assert.True (Lun.make "18 chrs is the max").LunOptNums.IsSome
        Assert.True (Lun.make "19 chrs is too many").LunOptNums.IsNone

    let [<Fact>] mixedLunComparisonTest () =
        let abc = Lun.make "abc" :> IComparable
        let cba = Lun.makeFast "cba" :> IComparable
        Assert.True (abc.CompareTo cba = -1)
        Assert.True (abc.CompareTo abc = 0)
        Assert.True (cba.CompareTo abc = 1)

    let [<Fact>] lunConcatenateTest () =
        let concatenated = Lun.make "abc" + Lun.make "cba"
        Assert.Equal (concatenated, Lun.make "abccba")