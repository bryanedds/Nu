namespace NUnit.Framework

[<RequireQualifiedAccess>]
module Assert =

    /// Generic implementation of AreEqual.
    let Equal<'a> (actual : 'a, expected : 'a) =
        Assert.AreEqual (actual, expected)

    /// Generic implementation of AreNotEqual.
    let NotEqual<'a> (actual : 'a, expected : 'a) =
        Assert.AreNotEqual (actual, expected)

    /// Generic implementation of AreSame.
    let Same<'a> (actual : 'a, expected : 'a) =
        Assert.AreSame (actual, expected)

    /// Generic implementation of AreNotSame.
    let NotSame<'a> (actual : 'a, expected : 'a) =
        Assert.AreNotSame (actual, expected)