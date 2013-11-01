module Nu.Screen
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DomainModel
open Nu.Entity
open Nu.Group

type [<StructuralEquality; NoComparison; CLIMutable>] Screen =
    { Id : Id
      GroupModels : Map<Lun, GroupModel> }

type [<StructuralEquality; NoComparison>] ScreenModel =
    | Screen of Screen
       
    static member private optChildModelFinder addressHead this =
        let screen = get this ScreenModel.screen
        Map.tryFind addressHead screen.GroupModels
    
    static member private childModelAdder addressHead this (child : GroupModel) =
        let screen = get this ScreenModel.screen
        let screen2 = { screen with GroupModels = Map.add addressHead child screen.GroupModels }
        set screen2 this ScreenModel.screen
    
    static member private childModelRemover addressHead this =
        let screen = get this ScreenModel.screen
        let screen2 = { screen with GroupModels = Map.remove addressHead screen.GroupModels }
        set screen2 this ScreenModel.screen

    static member private getChildWithLens this address lens =
        get (getChild ScreenModel.optChildModelFinder this address) lens

    static member private setChildWithLens child this address lens =
        let group = getChild ScreenModel.optChildModelFinder this address
        let group2 = set child group lens
        setChild ScreenModel.childModelAdder ScreenModel.childModelRemover this address group2

    static member private getOptChildWithLens this address lens =
        let optChild = getOptChild ScreenModel.optChildModelFinder this address
        match optChild with
        | None -> None
        | Some child -> Some (get child lens)

    static member private setOptChildWithLens optChild this address lens =
        match optChild with
        | None -> setOptChild ScreenModel.childModelAdder ScreenModel.childModelRemover this address None
        | Some child ->
            let optChildModel = getOptChild ScreenModel.optChildModelFinder this address
            match optChildModel with
            | None -> failwith "Cannot change a non-existent group."
            | Some childModel ->
                let childModel2 = set child childModel lens
                setChild ScreenModel.childModelAdder ScreenModel.childModelRemover this address childModel2

    static member screen =
        { Get = fun this ->
            match this with
            | Screen screen -> screen
          Set = fun screen this ->
            match this with
            | Screen _ -> Screen screen }

    static member groupModel address =
        { Get = fun this -> ScreenModel.getChildWithLens this address Lens.id
          Set = fun groupModel this -> ScreenModel.setChildWithLens groupModel this address Lens.id }

    static member optGroupModel address =
        { Get = fun this -> ScreenModel.getOptChildWithLens this address Lens.id
          Set = fun groupModel this -> ScreenModel.setOptChildWithLens groupModel this address Lens.id }

    static member group address =
        { Get = fun this -> ScreenModel.getChildWithLens this address GroupModel.group
          Set = fun group this -> ScreenModel.setChildWithLens group this address GroupModel.group }
    
    static member optGroup address =
        { Get = fun this -> ScreenModel.getOptChildWithLens this address GroupModel.group
          Set = fun optGroup this -> ScreenModel.setOptChildWithLens optGroup this address GroupModel.group }
    
    static member entityModel address = ScreenModel.groupModel [List.head address] >>| GroupModel.entityModel (List.tail address)
    static member optEntityModel address = ScreenModel.groupModel [List.head address] >>| GroupModel.optEntityModel (List.tail address)
    
    static member entity address = ScreenModel.groupModel [List.head address] >>| GroupModel.entity (List.tail address)
    static member optEntity address = ScreenModel.groupModel [List.head address] >>| GroupModel.optEntity (List.tail address)

    static member gui address = ScreenModel.groupModel [List.head address] >>| GroupModel.gui (List.tail address)
    static member optGui address = ScreenModel.groupModel [List.head address] >>| GroupModel.optGui (List.tail address)

    static member button address = ScreenModel.groupModel [List.head address] >>| GroupModel.button (List.tail address)
    static member optButton address = ScreenModel.groupModel [List.head address] >>| GroupModel.optButton (List.tail address)

    static member label address = ScreenModel.groupModel [List.head address] >>| GroupModel.label (List.tail address)
    static member optLabel address = ScreenModel.groupModel [List.head address] >>| GroupModel.optLabel (List.tail address)

    static member textBox address = ScreenModel.groupModel [List.head address] >>| GroupModel.textBox (List.tail address)
    static member optTextBox address = ScreenModel.groupModel [List.head address] >>| GroupModel.optTextBox (List.tail address)

    static member toggle address = ScreenModel.groupModel [List.head address] >>| GroupModel.toggle (List.tail address)
    static member optToggle address = ScreenModel.groupModel [List.head address] >>| GroupModel.optToggle (List.tail address)

    static member feeler address = ScreenModel.groupModel [List.head address] >>| GroupModel.feeler (List.tail address)
    static member optFeeler address = ScreenModel.groupModel [List.head address] >>| GroupModel.optFeeler (List.tail address)

    static member actor address = ScreenModel.groupModel [List.head address] >>| GroupModel.actor (List.tail address)
    static member optActor address = ScreenModel.groupModel [List.head address] >>| GroupModel.optActor (List.tail address)

    static member block address = ScreenModel.groupModel [List.head address] >>| GroupModel.block (List.tail address)
    static member optBlock address = ScreenModel.groupModel [List.head address] >>| GroupModel.optBlock (List.tail address)

    static member avatar address = ScreenModel.groupModel [List.head address] >>| GroupModel.avatar (List.tail address)
    static member optAvatar address = ScreenModel.groupModel [List.head address] >>| GroupModel.optAvatar (List.tail address)

    static member tileMap address = ScreenModel.groupModel [List.head address] >>| GroupModel.tileMap (List.tail address)
    static member optTileMap address = ScreenModel.groupModel [List.head address] >>| GroupModel.optTileMap (List.tail address)