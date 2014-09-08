namespace Nu
open System
open System.IO
open System.Xml
open Xunit
open Prime
open Nu.NuConstants
module Tests =

    /// TODO: test more stuff from Nu.

    type [<CLIMutable; NoComparison>] TestXtended =
        { Xtension : Xtension }

        static member (?) (this : TestXtended, memberName) =
            fun args ->
                Xtension.(?) (this.Xtension, memberName) args

        static member (?<-) (this : TestXtended, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member dispatchesAs dispatcherTargetType this dispatcherContainer =
            Xtension.dispatchesAs dispatcherTargetType this.Xtension dispatcherContainer

    type TestDispatcher () =
        member dispatcher.Init (xtn : Xtension, _ : IXDispatcherContainer) =
            xtn?InittedField <- 5
        member dispatcher.Test (xtn : Xtension, _ : IXDispatcherContainer) =
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
        xmlWriter.WriteStartElement RootNodeName
        write xmlWriter source
        xmlWriter.WriteEndElement ()
        xmlWriter.WriteEndDocument ()
        xmlWriter.Flush ()
        memoryStream :> Stream

    let readFromStream read (stream : Stream) target =
        let xmlReader = XmlReader.Create stream
        let xmlDocument = let emptyDoc = XmlDocument () in (emptyDoc.Load xmlReader; emptyDoc)
        let result = read (xmlDocument.SelectSingleNode RootNodeName) target
        result

    // globalization is fine since this object is stateless.
    let tdc = TestDispatcherContainer ()

    let [<Fact>] canAddField () =
        let xtn = Xtension.empty
        let xtn = xtn?TestField <- 5
        let fieldValue = xtn?TestField ()
        Assert.Equal (5, fieldValue)

    let [<Fact>] cantAddFieldWhenSealed () =
        let xtn = { Xtension.empty with Sealed = true }
        Assert.Throws<Exception> (fun () -> ignore <| xtn?TestField <- 0)

    let [<Fact>] cantAccessNonexistentField () =
        let xtn = { Xtension.empty with CanDefault = false }
        let xtn = xtn?TestField <- 5
        Assert.Throws<Exception> (fun () -> ignore <| xtn?TetField ())

    let [<Fact>] missingFieldReturnsDefault () =
        let xtn = Xtension.empty
        let xtn = xtn?TestField <- 0
        let fieldValue = xtn?MissingField ()
        Assert.Equal (0, fieldValue)

    let [<Fact>] canAddFieldViaContainingType () =
        let xtd = { Xtension = Xtension.empty }
        let xtd = xtd?TestField <- 5
        let fieldValue = xtd?TestField ()
        Assert.Equal (5, fieldValue)

    let [<Fact>] dispatchingWorks () =
        let xtn = { Xtension.empty with OptXDispatcherName = Some typeof<TestDispatcher>.Name }
        let xtn = xtn?Init (xtn, tdc) : Xtension
        let dispatchResult = xtn?Test (xtn, tdc)
        Assert.Equal (dispatchResult, 25)

    let [<Fact>] dispatchingFailsAppropriately () =
        let xtn = { Xtension.empty with OptXDispatcherName = Some typeof<TestDispatcher>.Name }
        Assert.Throws<Exception> (fun () -> ignore <| xtn?MissingDispatch tdc)

    let [<Fact>] xtensionSerializationWorks () =
        let xtn = Xtension.empty
        let xtn = xtn?TestField <- 5
        use stream = writeToStream (Serialization.writeXtension tautology) xtn
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtnRead = readFromStream (fun node _ -> Serialization.readXtension node) stream Xtension.empty
        Assert.Equal (xtn, xtnRead)

    let [<Fact>] xtensionSerializationViaContainingTypeWorks () =
        let xtd = { Xtension = { Xtension.empty with OptXDispatcherName = Some typeof<TestDispatcher>.Name }}
        let xtd = xtd?TestField <- 5
        use stream = writeToStream (Serialization.writeTargetProperties tautology) xtd
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtdRead = readFromStream (fun node target -> Serialization.readTargetProperties node target; target) stream { Xtension = Xtension.empty }
        Assert.Equal (xtd, xtdRead)