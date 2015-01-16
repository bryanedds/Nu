namespace Nu
open System
open Nu

[<AutoOpen>]
module SimulantAddressModule =

    /// Convert any type of address to a simulant's address.
    let atoua address = World.atoua address

    /// Convert any type of address to a screen's address.
    let atosa address = World.atosa address

    /// Convert any type of address to a group's address.
    let atoga address = World.atoga address

    /// Convert any type of address to an entity's address.
    let atoea address = World.atoea address

    /// Convert a group's address to an entity's by appending the entity's name at the end.
    let gatoea groupAddress entityName = World.gatoea groupAddress entityName

    /// Convert a screen's address to a group's by appending the group's name at the end.
    let satoga screenAddress groupName = World.satoga screenAddress groupName

    /// Convert a screen's address to an entity's by appending the group and entity's names at the end.
    let satoea screenAddress groupName entityName = World.satoea screenAddress groupName entityName

    /// Convert an entity's address to a group's by removing the entity's name from the end.
    let eatoga entityAddress = World.eatoga entityAddress

    /// Convert a group's address to a screen's by removing the group's name from the end.
    let gatosa groupAddress = World.gatosa groupAddress

    /// Convert a entity's address to a screen's by removing the group and entity's names from the end.
    let eatosa entityAddress = World.eatosa entityAddress