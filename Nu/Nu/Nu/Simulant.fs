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

    let setOptChild addChild removeChild address parent optChild =
        match optChild with
        | Some child -> addChild address parent child
        | None -> removeChild address parent

    let getChild optChildFinder address parent =
        Option.get <| optChildFinder address parent

    let setChild childAdder childRemover address parent child =
        setOptChild childAdder childRemover address parent (Some child)

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
        Address.changeType<Group, Entity> groupAddress ->- ltoa [entityName]

    let satoga screenAddress groupName =
        Address.changeType<Screen, Group> screenAddress ->- ltoa [groupName]

    let matosa gameAddress screenName =
        Address.changeType<Game, Screen> gameAddress ->- ltoa [screenName]

    let eatoga entityAddress =
        Address.take 2 entityAddress |> Address.changeType<Entity, Group>

    let gatosa groupAddress =
        Address.take 1 groupAddress |> Address.changeType<Group, Screen>

    let satoma screenAddress =
        Address.take 0 screenAddress |> Address.changeType<Screen, Game>