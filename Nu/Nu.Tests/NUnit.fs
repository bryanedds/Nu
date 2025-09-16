namespace NUnit.Framework

[<RequireQualifiedAccess>]
module Assert =

    /// Generic implementation of AreEqual.
    let Equal<'a> (expected : 'a, actual : 'a) =
        Assert.AreEqual (expected, actual)

    /// Generic implementation of AreNotEqual.
    let NotEqual<'a> (expected : 'a, actual : 'a) =
        Assert.AreNotEqual (expected, actual)

    /// Generic implementation of AreSame.
    let Same<'a> (expected : 'a, actual : 'a) =
        Assert.AreSame (expected, actual)

    /// Generic implementation of AreNotSame.
    let NotSame<'a> (expected : 'a, actual : 'a) =
        Assert.AreNotSame (expected, actual)