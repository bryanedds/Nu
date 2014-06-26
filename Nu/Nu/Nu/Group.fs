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
        member this.Init (dispatcherContainer : IXDispatcherContainer) : Group = this?Init (this, dispatcherContainer)
        member this.Register (address : Address, entities : Entity list, world : World) : World = this?Register (address, entities, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, world)

[<RequireQualifiedAccess>]
module Group =

    let makeDefaultUninitialized dispatcherName =
        { Group.Id = NuCore.getId ()
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

    let makeDefault dispatcherName dispatcherContainer =
        let group = makeDefaultUninitialized dispatcherName
        group.Init dispatcherContainer

    let writeToXml (writer : XmlWriter) group entities =
        writer.WriteStartElement typeof<Group>.Name
        Xtension.writeTargetProperties writer group
        Entity.writeManyToXml writer entities

    let readFromXml (groupNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName dispatcherContainer =
        let group = makeDefaultUninitialized defaultDispatcherName
        Xtension.readTargetXDispatcher groupNode group
        let group = group.Init dispatcherContainer
        Xtension.readTargetProperties groupNode group
        let entities = Entity.readManyFromXml (groupNode : XmlNode) defaultEntityDispatcherName dispatcherContainer
        (group, entities)