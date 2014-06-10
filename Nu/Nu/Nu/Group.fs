namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu

[<AutoOpen>]
module GroupModule =

    type Group with
        member this.Register (address : Address, entities : Entity list, world : World) : World = this?Register (address, entities, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, world)

[<RequireQualifiedAccess>]
module Group =

    let makeDefault defaultDispatcherName =
        { Group.Id = NuCore.getId ()
          FacetNamesNs = []
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some defaultDispatcherName; CanDefault = true; Sealed = false }}

    let writeToXml (writer : XmlWriter) group entities =
        writer.WriteStartElement typeof<Group>.Name
        Xtension.writePropertiesToXmlWriter writer group
        Entity.writeManyToXml writer entities

    let readFromXml (groupNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName seal world =
        let group = makeDefault defaultDispatcherName
        let entities = Entity.readManyFromXml (groupNode : XmlNode) defaultEntityDispatcherName seal world
        Xtension.readProperties groupNode group
        (group, entities)