module Nu.Group
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DomainModel
open Nu.Entity

type [<StructuralEquality; NoComparison; CLIMutable>] Group =
    { Id : Id
      EntityModels : Map<Lun, EntityModel> }

type [<StructuralEquality; NoComparison>] GroupModel =
    | Group of Group
       
    static member private optChildModelFinder addressHead this =
        let group = get this GroupModel.group
        Map.tryFind addressHead group.EntityModels
    
    static member private childModelAdder addressHead this (child : EntityModel) =
        let group = get this GroupModel.group
        let group2 = { group with EntityModels = Map.add addressHead child group.EntityModels }
        set group2 this GroupModel.group
    
    static member private childModelRemover addressHead this =
        let group = get this GroupModel.group
        let group2 = { group with EntityModels = Map.remove addressHead group.EntityModels }
        set group2 this GroupModel.group

    static member private getChildWithLens this address lens =
        get (getChild GroupModel.optChildModelFinder this address) lens

    static member private setChildWithLens child this address lens =
        let entity = getChild GroupModel.optChildModelFinder this address
        let entity2 = set child entity lens
        setChild GroupModel.childModelAdder GroupModel.childModelRemover this address entity2

    static member private getOptChildWithLens this address lens =
        let optChild = getOptChild GroupModel.optChildModelFinder this address
        match optChild with
        | None -> None
        | Some child -> Some (get child lens)

    static member private setOptChildWithLens optChild this address lens =
        match optChild with
        | None -> setOptChild GroupModel.childModelAdder GroupModel.childModelRemover this address None
        | Some child ->
            let optChildModel = getOptChild GroupModel.optChildModelFinder this address
            match optChildModel with
            | None -> failwith "Cannot change a non-existent entity."
            | Some childModel ->
                let childModel2 = set child childModel lens
                setChild GroupModel.childModelAdder GroupModel.childModelRemover this address childModel2

    static member group =
        { Get = fun this ->
            match this with
            | Group group -> group
          Set = fun group this ->
            match this with
            | Group _ -> Group group }

    static member entityModel address =
        { Get = fun this -> Option.get <| GroupModel.optChildModelFinder (List.head address) this
          Set = fun entity this -> GroupModel.childModelAdder (List.head address) this entity }
    
    static member optEntityModel address =
        { Get = fun this -> GroupModel.optChildModelFinder (List.head address) this
          Set = fun optEntity this -> match optEntity with None -> GroupModel.childModelRemover (List.head address) this | Some entity -> GroupModel.childModelAdder (List.head address) this entity }

    static member entity address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.entity
          Set = fun entity this -> GroupModel.setChildWithLens entity this address EntityModel.entity }
    
    static member optEntity address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.entity
          Set = fun optEntity this -> GroupModel.setOptChildWithLens optEntity this address EntityModel.entity }

    static member gui address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.gui
          Set = fun gui this -> GroupModel.setChildWithLens gui this address EntityModel.gui }
    
    static member optGui address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.gui
          Set = fun optGui this -> GroupModel.setOptChildWithLens optGui this address EntityModel.gui }

    static member button address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.button
          Set = fun button this -> GroupModel.setChildWithLens button this address EntityModel.button }
    
    static member optButton address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.button
          Set = fun button this -> GroupModel.setOptChildWithLens button this address EntityModel.button }

    static member label address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.label
          Set = fun label this -> GroupModel.setChildWithLens label this address EntityModel.label }
    
    static member optLabel address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.label
          Set = fun label this -> GroupModel.setOptChildWithLens label this address EntityModel.label }

    static member textBox address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.textBox
          Set = fun textBox this -> GroupModel.setChildWithLens textBox this address EntityModel.textBox }
    
    static member optTextBox address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.textBox
          Set = fun textBox this -> GroupModel.setOptChildWithLens textBox this address EntityModel.textBox }

    static member toggle address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.toggle
          Set = fun toggle this -> GroupModel.setChildWithLens toggle this address EntityModel.toggle }
    
    static member optToggle address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.toggle
          Set = fun toggle this -> GroupModel.setOptChildWithLens toggle this address EntityModel.toggle }

    static member feeler address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.feeler
          Set = fun feeler this -> GroupModel.setChildWithLens feeler this address EntityModel.feeler }
    
    static member optFeeler address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.feeler
          Set = fun feeler this -> GroupModel.setOptChildWithLens feeler this address EntityModel.feeler }

    static member actor address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.actor
          Set = fun actor this -> GroupModel.setChildWithLens actor this address EntityModel.actor }
    
    static member optActor address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.optActor
          Set = fun optActor this -> GroupModel.setOptChildWithLens optActor this address EntityModel.optActor }

    static member block address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.block
          Set = fun block this -> GroupModel.setChildWithLens block this address EntityModel.block }
    
    static member optBlock address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.block
          Set = fun block this -> GroupModel.setOptChildWithLens block this address EntityModel.block }

    static member avatar address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.avatar
          Set = fun avatar this -> GroupModel.setChildWithLens avatar this address EntityModel.avatar }
    
    static member optAvatar address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.avatar
          Set = fun avatar this -> GroupModel.setOptChildWithLens avatar this address EntityModel.avatar }

    static member tileMap address =
        { Get = fun this -> GroupModel.getChildWithLens this address EntityModel.tileMap
          Set = fun tileMap this -> GroupModel.setChildWithLens tileMap this address EntityModel.tileMap }
    
    static member optTileMap address =
        { Get = fun this -> GroupModel.getOptChildWithLens this address EntityModel.tileMap
          Set = fun tileMap this -> GroupModel.setOptChildWithLens tileMap this address EntityModel.tileMap }