namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open FSharpx
open FSharpx.Lens.Operators
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module EntityModule =

    type Entity with

        member entity.Init (dispatcherContainer : IXDispatcherContainer) : Entity = entity?Init (entity, dispatcherContainer)
        member entity.Register (address : Address, world : World) : World = entity?Register (address, world)
        member entity.Unregister (address : Address, world : World) : World = entity?Unregister (address, world)
        member entity.GetPickingPriority (world : World) : single = entity?GetPickingPriority (entity, world)

[<RequireQualifiedAccess>]
module Entity =

    let makeDefaultUninitialized dispatcherName optName =
        let id = NuCore.makeId ()
        { Id = id
          Name = match optName with None -> string id | Some name -> name
          Visible = true
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

    let makeDefault dispatcherName optName dispatcherContainer =
        let entity = makeDefaultUninitialized dispatcherName optName
        entity.Init dispatcherContainer

    let writeToXml (writer : XmlWriter) entity =
        writer.WriteStartElement typeof<Entity>.Name
        Xtension.writeTargetProperties writer entity
        writer.WriteEndElement ()

    let writeManyToXml (writer : XmlWriter) (entities : Map<_, _>) =
        for entityKvp in entities do
            writeToXml writer entityKvp.Value

    let readFromXml (entityNode : XmlNode) defaultDispatcherName dispatcherContainer =
        let entity = makeDefaultUninitialized defaultDispatcherName None
        Xtension.readTargetXDispatcher entityNode entity
        let entity = entity.Init dispatcherContainer
        Xtension.readTargetProperties entityNode entity
        entity

    let readManyFromXml (parentNode : XmlNode) defaultDispatcherName dispatcherContainer =
        let entityNodes = parentNode.SelectNodes "Entity"
        let entities =
            Seq.map
                (fun entityNode -> readFromXml entityNode defaultDispatcherName dispatcherContainer)
                (enumerable entityNodes)
        Seq.toList entities