// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.IO
open System.Xml
open Xunit
open Prime
open Nu
open Nu.Constants
module XtensionTests =

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

    let readFromStream read (stream : Stream) target : unit =
        let xmlReader = XmlReader.Create stream
        let xmlDocument = let emptyDoc = XmlDocument () in (emptyDoc.Load xmlReader; emptyDoc)
        read (xmlDocument.SelectSingleNode RootNodeName) target

    let [<Fact>] canAddField () =
        let xtn = Xtension.empty
        let xtn = xtn?TestField <- 5
        let fieldValue = xtn?TestField
        Assert.Equal (5, fieldValue)

#if DEBUG
    let [<Fact>] cantAddFieldWhenSealed () =
        let xtn = Xtension.safe
        Assert.Throws<Exception> (fun () -> ignore <| xtn?TestField <- 0)
#endif

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

    let [<Fact>] xtensionSerializationViaContainingTypeWorks () =
        let xtd = { Xtension = Xtension.mixed }
        let xtd = xtd?TestField <- 5
        use stream = writeToStream (Reflection.writeMemberValuesFromTarget tautology3) xtd
        ignore <| stream.Seek (0L, SeekOrigin.Begin)
        let xtdRead = { xtd with Xtension = xtd.Xtension } // hacky copy
        readFromStream (fun node target -> Reflection.readMemberValuesToTarget node target) stream { Xtension = Xtension.mixed }
        Assert.Equal (xtd, xtdRead)