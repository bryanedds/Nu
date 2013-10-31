module Nu.Group
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DataModel
open Nu.Entity

type [<StructuralEquality; NoComparison; CLIMutable>] GroupRcd =
    { Id : Id
      Entities : Map<Lun, Entity> }

type [<StructuralEquality; NoComparison>] Group =
    | Group of GroupRcd
       
    static member private optChildFinder addressHead this =
        let groupRcd = get this Group.groupRcd
        Map.tryFind addressHead groupRcd.Entities
    
    static member private childAdder addressHead this (child : Entity) =
        let groupRcd = get this Group.groupRcd
        let groupRcd2 = { groupRcd with Entities = Map.add addressHead child groupRcd.Entities }
        set groupRcd2 this Group.groupRcd
    
    static member private childRemover addressHead this =
        let groupRcd = get this Group.groupRcd
        let groupRcd2 = { groupRcd with Entities = Map.remove addressHead groupRcd.Entities }
        set groupRcd2 this Group.groupRcd

    static member private getChildWithLens this address lens =
        get (getChild Group.optChildFinder this address) lens

    static member private setChildWithLens child this address lens =
        let entity = getChild Group.optChildFinder this address
        let entity2 = set child entity lens
        setChild Group.childAdder Group.childRemover this address entity2

    static member private getOptChildWithLens this address lens =
        let optChild = getOptChild Group.optChildFinder this address
        match optChild with
        | None -> None
        | Some child -> Some (get child lens)

    static member private setOptChildWithLens optChildRcd this address lens =
        match optChildRcd with
        | None -> setOptChild Group.childAdder Group.childRemover this address None
        | Some childRcd ->
            let optChild = getOptChild Group.optChildFinder this address
            match optChild with
            | None -> failwith "Cannot change a non-existent entity."
            | Some child ->
                let child2 = set childRcd child lens
                setChild Group.childAdder Group.childRemover this address child2

    static member groupRcd =
        { Get = fun this ->
            match this with
            | Group groupRcd -> groupRcd
          Set = fun groupRcd this ->
            match this with
            | Group _ -> Group groupRcd }

    static member entityRcd address =
        { Get = fun this -> Group.getChildWithLens this address Entity.entityRcd
          Set = fun entityRcd this -> Group.setChildWithLens entityRcd this address Entity.entityRcd }
    
    static member optEntityRcd address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.entityRcd
          Set = fun optEntityRcd this -> Group.setOptChildWithLens optEntityRcd this address Entity.entityRcd }

    static member gui address =
        { Get = fun this -> Group.getChildWithLens this address Entity.gui
          Set = fun entityRcd this -> Group.setChildWithLens entityRcd this address Entity.gui }
    
    static member optGui address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.gui
          Set = fun optGui this -> Group.setOptChildWithLens optGui this address Entity.gui }

    static member button address =
        { Get = fun this -> Group.getChildWithLens this address Entity.button
          Set = fun button this -> Group.setChildWithLens button this address Entity.button }
    
    static member optButton address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.button
          Set = fun button this -> Group.setOptChildWithLens button this address Entity.button }

    static member label address =
        { Get = fun this -> Group.getChildWithLens this address Entity.label
          Set = fun label this -> Group.setChildWithLens label this address Entity.label }
    
    static member optLabel address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.label
          Set = fun label this -> Group.setOptChildWithLens label this address Entity.label }

    static member textBox address =
        { Get = fun this -> Group.getChildWithLens this address Entity.textBox
          Set = fun textBox this -> Group.setChildWithLens textBox this address Entity.textBox }
    
    static member optTextBox address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.textBox
          Set = fun textBox this -> Group.setOptChildWithLens textBox this address Entity.textBox }

    static member toggle address =
        { Get = fun this -> Group.getChildWithLens this address Entity.toggle
          Set = fun toggle this -> Group.setChildWithLens toggle this address Entity.toggle }
    
    static member optToggle address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.toggle
          Set = fun toggle this -> Group.setOptChildWithLens toggle this address Entity.toggle }

    static member feeler address =
        { Get = fun this -> Group.getChildWithLens this address Entity.feeler
          Set = fun feeler this -> Group.setChildWithLens feeler this address Entity.feeler }
    
    static member optFeeler address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.feeler
          Set = fun feeler this -> Group.setOptChildWithLens feeler this address Entity.feeler }

    static member actor address =
        { Get = fun this -> Group.getChildWithLens this address Entity.actor
          Set = fun actor this -> Group.setChildWithLens actor this address Entity.actor }
    
    static member optActor address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.optActor
          Set = fun optActor this -> Group.setOptChildWithLens optActor this address Entity.optActor }

    static member block address =
        { Get = fun this -> Group.getChildWithLens this address Entity.block
          Set = fun block this -> Group.setChildWithLens block this address Entity.block }
    
    static member optBlock address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.block
          Set = fun block this -> Group.setOptChildWithLens block this address Entity.block }

    static member avatar address =
        { Get = fun this -> Group.getChildWithLens this address Entity.avatar
          Set = fun avatar this -> Group.setChildWithLens avatar this address Entity.avatar }
    
    static member optAvatar address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.avatar
          Set = fun avatar this -> Group.setOptChildWithLens avatar this address Entity.avatar }

    static member tileMap address =
        { Get = fun this -> Group.getChildWithLens this address Entity.tileMap
          Set = fun tileMap this -> Group.setChildWithLens tileMap this address Entity.tileMap }
    
    static member optTileMap address =
        { Get = fun this -> Group.getOptChildWithLens this address Entity.tileMap
          Set = fun tileMap this -> Group.setOptChildWithLens tileMap this address Entity.tileMap }