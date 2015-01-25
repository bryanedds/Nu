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
    let atoua address = Address.changeType<'a, SimulantState> address

    /// Convert any type of address to a screen's address.
    let atosa address = Address.changeType<'a, ScreenState> address

    /// Convert any type of address to a group's address.
    let atoga address = Address.changeType<'a, GroupState> address

    /// Convert any type of address to an entity's address.
    let atoea address = Address.changeType<'a, EntityState> address

    /// Convert a group's address to an entity's by appending the entity's name at the end.
    let gatoea groupAddress entityName = Address.changeType<GroupState, EntityState> groupAddress ->- ntoa entityName

    /// Convert a screen's address to a group's by appending the group's name at the end.
    let satoga screenAddress groupName = Address.changeType<ScreenState, GroupState> screenAddress ->- ntoa groupName

    /// Convert a screen's address to an entity's by appending the group and entity's names at the end.
    let satoea screenAddress groupName entityName = gatoea (satoga screenAddress groupName) entityName

    /// Convert an entity's address to a group's by removing the entity's name from the end.
    let eatoga entityAddress = Address.take<EntityState, GroupState> 2 entityAddress

    /// Convert a group's address to a screen's by removing the group's name from the end.
    let gatosa groupAddress = Address.take<GroupState, ScreenState> 1 groupAddress

    /// Convert a entity's address to a screen's by removing the group and entity's names from the end.
    let eatosa entityAddress = Address.take<EntityState, ScreenState> 1 entityAddress