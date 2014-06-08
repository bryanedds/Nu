namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuCore
open Nu.Sim
open Nu.Entity

[<AutoOpen>]
module GroupModule =

    type Group with
        member this.Register (address : Address, entities : Entity list, world : World) : World = this?Register (address, entities, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, world)

module Group =

    let groupId =
        { Get = fun group -> group.Id
          Set = fun value group -> { group with Id = value }}

    let groupXtension =
        { Get = fun group -> group.Xtension
          Set = fun value group -> { group with Xtension = value }}

    let groupXField fieldName =
        { Get = fun (group : Group) -> (?) group fieldName
          Set = fun value group -> (?<-) group fieldName value }

    let makeDefaultGroup dispatcherName =
        { Group.Id = getNuId ()
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

    let writeGroupEntitiesToXml (writer : XmlWriter) (entities : Map<string, Entity>) =
        for entityKvp in entities do
            writeEntityToXml writer entityKvp.Value

    let writeGroupToXml (writer : XmlWriter) group entities =
        writer.WriteStartElement typeof<Group>.Name
        Xtension.writePropertiesToXmlWriter writer group
        writeGroupEntitiesToXml writer entities

    let readEntitiesFromXml (groupNode : XmlNode) defaultDispatcherName seal world =
        let entityNodes = groupNode.SelectNodes "Entity"
        let entities =
            Seq.map
                (fun entityNode -> readEntityFromXml entityNode defaultDispatcherName seal world)
                (enumerable entityNodes)
        Seq.toList entities

    let readGroupFromXml (groupNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName seal world =
        let group = makeDefaultGroup defaultDispatcherName
        let entities = readEntitiesFromXml (groupNode : XmlNode) defaultEntityDispatcherName seal world
        Xtension.readProperties groupNode group
        (group, entities)