namespace Nu
open System
open System.IO
open System.Xml
open Xunit
open Prime
open Nu.Constants
module Tests =

    /// TODO: test more stuff from Nu.

    type [<CLIMutable; NoComparison>] TestXtended =
        { Xtension : Xtension }

        static member (?) (this : TestXtended, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : TestXtended, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

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

    let [<Fact>] canAddField () =
        let xtn = Xtension.empty
        let xtn = xtn?TestField <- 5
        let fieldValue = xtn?TestField
        Assert.Equal (5, fieldValue)

    let [<Fact>] cantAddFieldWhenSealed () =
        let xtn = Xtension.safe
        Assert.Throws<Exception> (fun () -> ignore <| xtn?TestField <- 0)

    let [<Fact>] cantAccessNonexistentField () =
        let xtn = Xtension.mixed
        let xtn = xtn?TestField <- 5
        Assert.Throws<Exception> (fun () -> ignore <| xtn?TetField)

    let [<Fact>] missingFieldReturnsDefault () =
        let xtn = Xtension.empty
        let xtn = xtn?TestField <- 0
        let fieldValue = xtn?MissingField
        Assert.Equal (0, fieldValue)

    let [<Fact>] canAddFieldViaContainingType () =
        let xtd = { Xtension = Xtension.empty }
        let xtd = xtd?TestField <- 5
        let fieldValue = xtd?TestField
        Assert.Equal (5, fieldValue)

    let [<Fact>] xtensionSerializationWorks () =
        let xtn = Xtension.mixed
        let xtn = xtn?TestField <- 5
        let xtn = { xtn with Sealed = true } // NOTE: equality semantics for Xtension include safety configuration info
        use stream = writeToStream (Serialization.writeXtension tautology) xtn
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtnRead = readFromStream (fun node _ -> Serialization.readXtension node) stream <| Xtension.mixed
        Assert.Equal (xtn, xtnRead)

    let [<Fact>] xtensionSerializationViaContainingTypeWorks () =
        let xtd = { Xtension = Xtension.mixed }
        let xtd = xtd?TestField <- 5
        use stream = writeToStream (Serialization.writePropertiesFromTarget tautology) xtd
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtdRead = readFromStream (fun node target -> Serialization.readPropertiesToTarget node target; target) stream { Xtension = Xtension.mixed }
        Assert.Equal (xtd, xtdRead)