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

    let toEntity simulant =
        match simulant with
        | Entity entity -> entity
        | Group _ | Screen _ | Game _ -> failwith "Invalid conversion of simulant to entity."

    let toGroup simulant =
        match simulant with
        | Group group -> group
        | Entity _ | Screen _ | Game _ -> failwith "Invalid conversion of simulant to group."

    let toScreen simulant =
        match simulant with
        | Screen screen -> screen
        | Entity _ | Group _ | Game _ -> failwith "Invalid conversion of simulant to screen."

    let toGame simulant =
        match simulant with
        | Game game -> game
        | Entity _ | Group _ | Screen _ -> failwith "Invalid conversion of simulant to game."

    let toGeneric<'s> simulant =
        let s = typeof<'s>
        // OPTIMIZATION: Entity type is most common and therefore checked first.
        if s = typeof<Entity> then toEntity simulant :> obj :?> 's
        elif s = typeof<Group> then toGroup simulant :> obj :?> 's
        elif s = typeof<Screen> then toScreen simulant :> obj :?> 's
        elif s = typeof<Game> then toGame simulant :> obj :?> 's
        elif s = typeof<obj> then simulant :> obj :?> 's
        else failwith <| "Invalid simulation type '" + s.Name + "'."

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