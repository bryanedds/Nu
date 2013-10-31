module Nu.Screen
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DataModel
open Nu.Entity
open Nu.Group

type [<StructuralEquality; NoComparison; CLIMutable>] ScreenRcd =
    { Id : Id
      Groups : Map<Lun, Group> }

type [<StructuralEquality; NoComparison>] Screen =
    | Screen of ScreenRcd
       
    static member private optChildFinder addressHead this =
        let screenRcd = get this Screen.screenRcd
        Map.tryFind addressHead screenRcd.Groups
    
    static member private childAdder addressHead this (child : Group) =
        let screenRcd = get this Screen.screenRcd
        let screenRcd2 = { screenRcd with Groups = Map.add addressHead child screenRcd.Groups }
        set screenRcd2 this Screen.screenRcd
    
    static member private childRemover addressHead this =
        let screenRcd = get this Screen.screenRcd
        let screenRcd2 = { screenRcd with Groups = Map.remove addressHead screenRcd.Groups }
        set screenRcd2 this Screen.screenRcd

    static member private getChildWithLens this address lens =
        get (getChild Screen.optChildFinder this address) lens

    static member private setChildWithLens child this address lens =
        let group = getChild Screen.optChildFinder this address
        let group2 = set child group lens
        setChild Screen.childAdder Screen.childRemover this address group2

    static member private getOptChildWithLens this address lens =
        let optChild = getOptChild Screen.optChildFinder this address
        match optChild with
        | None -> None
        | Some child -> Some (get child lens)

    static member private setOptChildWithLens optChildRcd this address lens =
        match optChildRcd with
        | None -> setOptChild Screen.childAdder Screen.childRemover this address None
        | Some childRcd ->
            let optChild = getOptChild Screen.optChildFinder this address
            match optChild with
            | None -> failwith "Cannot change a non-existent group."
            | Some child ->
                let child2 = set childRcd child lens
                setChild Screen.childAdder Screen.childRemover this address child2

    static member screenRcd =
        { Get = fun this ->
            match this with
            | Screen screenRcd -> screenRcd
          Set = fun screenRcd this ->
            match this with
            | Screen _ -> Screen screenRcd }

    static member group address =
        { Get = fun this -> Screen.getChildWithLens this address Lens.id
          Set = fun groupRcd this -> Screen.setChildWithLens groupRcd this address Lens.id }

    static member groupRcd address =
        { Get = fun this -> Screen.getChildWithLens this address Group.groupRcd
          Set = fun groupRcd this -> Screen.setChildWithLens groupRcd this address Group.groupRcd }
    
    static member optGroupRcd address =
        { Get = fun this -> Screen.getOptChildWithLens this address Group.groupRcd
          Set = fun optGroupRcd this -> Screen.setOptChildWithLens optGroupRcd this address Group.groupRcd }
    
    static member entityRcd address = Screen.group [List.head address] >>| Group.entityRcd (List.tail address)
    static member optEntityRcd address = Screen.group [List.head address] >>| Group.optEntityRcd (List.tail address)

    static member gui address = Screen.group [List.head address] >>| Group.gui (List.tail address)
    static member optGui address = Screen.group [List.head address] >>| Group.optGui (List.tail address)

    static member button address = Screen.group [List.head address] >>| Group.button (List.tail address)
    static member optButton address = Screen.group [List.head address] >>| Group.optButton (List.tail address)

    static member label address = Screen.group [List.head address] >>| Group.label (List.tail address)
    static member optLabel address = Screen.group [List.head address] >>| Group.optLabel (List.tail address)

    static member textBox address = Screen.group [List.head address] >>| Group.textBox (List.tail address)
    static member optTextBox address = Screen.group [List.head address] >>| Group.optTextBox (List.tail address)

    static member toggle address = Screen.group [List.head address] >>| Group.toggle (List.tail address)
    static member optToggle address = Screen.group [List.head address] >>| Group.optToggle (List.tail address)

    static member feeler address = Screen.group [List.head address] >>| Group.feeler (List.tail address)
    static member optFeeler address = Screen.group [List.head address] >>| Group.optFeeler (List.tail address)

    static member actor address = Screen.group [List.head address] >>| Group.actor (List.tail address)
    static member optActor address = Screen.group [List.head address] >>| Group.optActor (List.tail address)

    static member block address = Screen.group [List.head address] >>| Group.block (List.tail address)
    static member optBlock address = Screen.group [List.head address] >>| Group.optBlock (List.tail address)

    static member avatar address = Screen.group [List.head address] >>| Group.avatar (List.tail address)
    static member optAvatar address = Screen.group [List.head address] >>| Group.optAvatar (List.tail address)

    static member tileMap address = Screen.group [List.head address] >>| Group.tileMap (List.tail address)
    static member optTileMap address = Screen.group [List.head address] >>| Group.optTileMap (List.tail address)