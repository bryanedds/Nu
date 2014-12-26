namespace Nu
open System
open Nu

[<RequireQualifiedAccess>]
module Simulant =

    let getOptChild optChildFinder address parent =
        let optChild = optChildFinder address parent
        match optChild with
        | Some child -> Some child
        | None -> None

    let setOptChild addChild removeChild optChild address parent =
        match optChild with
        | Some child -> addChild address parent child
        | None -> removeChild address parent

    let getChild optChildFinder address parent =
        Option.get <| optChildFinder address parent

    let setChild childAdder childRemover child address parent =
        setOptChild childAdder childRemover (Some child) address parent

[<AutoOpen>]
module SimulantAddressModule =

    let atoea address =
        Address.changeType<'t, Entity> address

    let atoga address =
        Address.changeType<'t, Group> address

    let atosa address =
        Address.changeType<'t, Screen> address

    let atoma address =
        Address.changeType<'t, Game> address

    let atoua address =
        Address.changeType<'t, Simulant> address

    let gatoea groupAddress entityName =
        Address.changeType<Group, Entity> groupAddress ->- ntoa entityName

    let satoga screenAddress groupName =
        Address.changeType<Screen, Group> screenAddress ->- ntoa groupName

    let satoea screenAddress groupName entityName =
        gatoea (satoga screenAddress groupName) entityName

    let eatoga entityAddress =
        Address.take<Entity, Group> 2 entityAddress

    let gatosa groupAddress =
        Address.take<Group, Screen> 1 groupAddress

    let eatosa entityAddress =
        Address.take<Entity, Screen> 1 entityAddress