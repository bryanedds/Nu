// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open System.IO
open System.Xml
open Xunit
open Prime
module Tests =

    type [<CLIMutable; NoComparison>] TestXtended =
        { Xtension : Xtension }

        static member (?) (this : TestXtended, memberName) =
            fun args ->
                Xtension.(?) (this, this.Xtension, memberName) args

        static member (?<-) (this : TestXtended, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    type TestDispatcher () =
        member dispatcher.Init (xtn : Xtension, _ : IXDispatcherContainer) =
            xtn?InittedField <- 5
        member dispatcher.Dispatch (xtn : Xtension, _ : IXDispatcherContainer) =
            xtn?InittedField () * 5

    type TestDispatcherContainer () =
        let testDispatcher = (TestDispatcher ()) :> obj
        let testDispatchers = Map.singleton typeof<TestDispatcher>.Name testDispatcher
        interface IXDispatcherContainer with
            member this.GetDispatchers () = testDispatchers

    let writeToStream write source =
        let memoryStream = new MemoryStream ()
        let xmlWriterSettings = XmlWriterSettings ()
        let xmlWriter = XmlWriter.Create (memoryStream, xmlWriterSettings)
        xmlWriter.WriteStartDocument ()
        xmlWriter.WriteStartElement "Root"
        write xmlWriter source
        xmlWriter.WriteEndElement ()
        xmlWriter.WriteEndDocument ()
        xmlWriter.Flush ()
        memoryStream :> Stream

    let readFromStream read (stream : Stream) target =
        let xmlReader = XmlReader.Create stream
        let xmlDocument = let emptyDoc = XmlDocument () in (emptyDoc.Load xmlReader; emptyDoc)
        let result' = read (xmlDocument.SelectSingleNode "Root") target
        result'

    // globalization is fine since this object is stateless.
    let tdc = TestDispatcherContainer ()

    let [<Fact>] canAddField () =
        let xtn = Xtension.empty
        let xtn' = xtn?TestField <- 5
        let fieldValue = xtn'?TestField ()
        Assert.Equal (5, fieldValue)

    let [<Fact>] cantAddFieldWhenSealed () =
        let xtn = { Xtension.empty with IsSealed = true }
        Assert.Throws<Exception> (fun () -> ignore <| xtn?TestField <- 0)

    let [<Fact>] cantAccessNonexistentField () =
        let xtn = { Xtension.empty with CanDefault = false }
        let xtn' = xtn?TestField <- 5
        Assert.Throws<Exception> (fun () -> ignore <| xtn'?TetField ())

    let [<Fact>] missingFieldReturnsDefault () =
        let xtn = Xtension.empty
        let xtn' = xtn?TestField <- 0
        let fieldValue = xtn'?MissingField ()
        Assert.Equal (0, fieldValue)

    let [<Fact>] canAddFieldViaContainingType () =
        let xtd = { Xtension = Xtension.empty }
        let xtd' = xtd?TestField <- 5
        let fieldValue = xtd'?TestField ()
        Assert.Equal (5, fieldValue)

    let [<Fact>] dispatchingWorks () =
        let xtn = { Xtension.empty with OptXTypeName = Some typeof<TestDispatcher>.Name }
        let xtn' = xtn?Init tdc : Xtension
        let dispatchResult = xtn'?Dispatch tdc
        Assert.Equal (dispatchResult, 25)

    let [<Fact>] dispatchingFailsAppropriately () =
        let xtn = { Xtension.empty with OptXTypeName = Some typeof<TestDispatcher>.Name }
        Assert.Throws<Exception> (fun () -> ignore <| xtn?MissingDispatch tdc)

    let [<Fact>] xtensionSerializationWorks () =
        let xtn = Xtension.empty
        let xtn' = xtn?TestField <- 5
        use stream = writeToStream Xtension.writeToXmlWriter xtn'
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtn'' = readFromStream (fun node _ -> Xtension.read node) stream xtn'
        Assert.Equal (xtn', xtn'')

    let [<Fact>] xtensionSerializationViaContainingTypeWorks () =
        let xtd = { Xtension = { Xtension.empty with OptXTypeName = Some typeof<TestDispatcher>.Name }}
        let xtd' = xtd?TestField <- 5
        use stream = writeToStream Xtension.writePropertiesToXmlWriter xtd'
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtd'' = readFromStream (fun node target -> Xtension.readProperties node target; target) stream xtd
        Assert.Equal (xtd', xtd'')