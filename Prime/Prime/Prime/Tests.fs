// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open System.IO
open System.Xml
open Xunit
open Prime
module Tests =

    type TestDispatcher () =
        member this.TestDispatch (_ : IXDispatcherContainer) = 0

    type TestDispatcherContainer () =
        let testDispatcher = (TestDispatcher ()) :> obj
        let testDispatchers = Map.singleton typeof<TestDispatcher>.Name testDispatcher
        interface IXDispatcherContainer with
            member this.GetDispatchers () = testDispatchers

    type [<CLIMutable; NoComparison>] XtendedType =
        { Xtension : Xtension }

        static member (?) (this : XtendedType, memberName) =
            fun args ->
                (?) this.Xtension memberName args

        static member (?<-) (this : XtendedType, memberName, value) =
            let xtension = Xtension.op_DynamicAssignment (this.Xtension, memberName, value)
            { this with Xtension = xtension }
            
    let writeToStream writeFn source =
        let memoryStream = new MemoryStream ()
        let xmlWriterSettings = XmlWriterSettings ()
        let xmlWriter = XmlWriter.Create (memoryStream, xmlWriterSettings)
        xmlWriter.WriteStartDocument ()
        xmlWriter.WriteStartElement "Root"
        writeFn xmlWriter source
        xmlWriter.WriteEndElement ()
        xmlWriter.WriteEndDocument ()
        xmlWriter.Flush ()
        memoryStream :> Stream

    let readFromStream readFn (stream : Stream) target =
        let xmlReader = XmlReader.Create stream
        let xmlDocument = let emptyDoc = XmlDocument () in (emptyDoc.Load xmlReader; emptyDoc)
        let result' = readFn (xmlDocument.SelectSingleNode "Root") target
        result'

    // globalization is fine since this object is stateless.
    let tdc = TestDispatcherContainer ()

    let [<Fact>] canAddField () =
        let xtn = Xtension.empty?TestField <- 5
        Assert.Equal (5, xtn?TestField ())

    let [<Fact>] cantAddFieldWhenSealed () =
        let xtn = { OptXTypeName = None; XFields = Map.empty; IsSealed = true }
        Assert.Throws<Exception> (fun () -> ignore <| xtn?TestField <- 0)

    let [<Fact>] missingFieldReturnsDefault () =
        let xtn = Xtension.empty?TestField <- 0
        Assert.Equal (0, xtn?MissingField ())

    let [<Fact>] dispatchingWorks () =
        let xtn = { OptXTypeName = Some typeof<TestDispatcher>.Name; XFields = Map.empty; IsSealed = true }
        Assert.Equal (xtn?TestDispatch tdc, 0)

    let [<Fact>] dispatchingFailsAppropriately () =
        let xtn = { OptXTypeName = Some typeof<TestDispatcher>.Name; XFields = Map.empty; IsSealed = true }
        Assert.Throws<Exception> (fun () -> ignore <| xtn?MissingDispatch tdc)

    let [<Fact>] xtensionSerializationWorks () =
        let xtn = { OptXTypeName = Some typeof<TestDispatcher>.Name; XFields = Map.empty; IsSealed = false }
        let xtn' = xtn?TestField <- 5
        use stream = writeToStream Xtension.writeToXmlWriter xtn'
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtn'' = readFromStream (fun s _ -> Xtension.read s) stream xtn
        Assert.Equal (xtn', xtn'')

    let [<Fact>] containedXtensionSerializationWorks () =
        let xtd = { Xtension = { OptXTypeName = Some typeof<TestDispatcher>.Name; XFields = Map.empty; IsSealed = false }}
        let xtd' = xtd?TestField <- 5
        use stream = writeToStream Xtension.writePropertiesToXmlWriter xtd'
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtd'' = readFromStream (fun s x -> Xtension.readProperties s x; x) stream xtd
        Assert.Equal (xtd', xtd'')