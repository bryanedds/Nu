// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open OpenTK
open Nu
open Nu.Constants

[<AutoOpen>]
module WorldAddressModule =

    /// Convert any type of address to a simulant's address.
    let atoua address = Address.changeType<'a, Simulant> address

    /// Convert any type of address to a screen's address.
    let atosa address = Address.changeType<'a, Screen> address

    /// Convert any type of address to a group's address.
    let atoga address = Address.changeType<'a, Group> address

    /// Convert any type of address to an entity's address.
    let atoea address = Address.changeType<'a, Entity> address

    /// Convert a group's address to an entity's by appending the entity's name at the end.
    let gatoea groupAddress entityName = Address.changeType<Group, Entity> groupAddress ->- ntoa entityName

    /// Convert a screen's address to a group's by appending the group's name at the end.
    let satoga screenAddress groupName = Address.changeType<Screen, Group> screenAddress ->- ntoa groupName

    /// Convert a screen's address to an entity's by appending the group and entity's names at the end.
    let satoea screenAddress groupName entityName = gatoea (satoga screenAddress groupName) entityName

    /// Convert an entity's address to a group's by removing the entity's name from the end.
    let eatoga entityAddress = Address.take<Entity, Group> 2 entityAddress

    /// Convert a group's address to a screen's by removing the group's name from the end.
    let gatosa groupAddress = Address.take<Group, Screen> 1 groupAddress

    /// Convert a entity's address to a screen's by removing the group and entity's names from the end.
    let eatosa entityAddress = Address.take<Entity, Screen> 1 entityAddress