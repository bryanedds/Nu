module Nu.Game
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DataModel
open Nu.Entity
open Nu.Group
open Nu.Screen

// WISDOM:
//
// A simulation that would put physics on another thread should likely do so in a different app
// domain with communication via .NET remoting to make 100% sure that no sharing is happening.
//
// NOTE: for simulation types, value semantics are preferred over open semantics as it eases
// serialization and other forms of automation. However, perhaps there is a way to get both...

type [<StructuralEquality; NoComparison; CLIMutable>] GameRcd =
    { Id : Id
      Screens : Map<Lun, Screen>
      OptSelectedScreenAddress : Address option }
        
type [<StructuralEquality; NoComparison>] Game =
    | Game of GameRcd
       
    static member private optChildFinder addressHead this =
        let gameRcd = get this Game.gameRcd
        Map.tryFind addressHead gameRcd.Screens
    
    static member private childAdder addressHead this (child : Screen) =
        let gameRcd = get this Game.gameRcd
        let gameRcd2 = { gameRcd with Screens = Map.add addressHead child gameRcd.Screens }
        set gameRcd2 this Game.gameRcd
    
    static member private childRemover addressHead this =
        let gameRcd = get this Game.gameRcd
        let gameRcd2 = { gameRcd with Screens = Map.remove addressHead gameRcd.Screens }
        set gameRcd2 this Game.gameRcd

    static member private getChildWithLens this address lens =
        get (getChild Game.optChildFinder this address) lens

    static member private setChildWithLens child this address lens =
        let screen = getChild Game.optChildFinder this address
        let screen2 = set child screen lens
        setChild Game.childAdder Game.childRemover this address screen2

    static member private getOptChildWithLens this address lens =
        let optChild = getOptChild Game.optChildFinder this address
        match optChild with
        | None -> None
        | Some child -> Some (get child lens)

    static member private setOptChildWithLens optChildRcd this address lens =
        match optChildRcd with
        | None -> setOptChild Game.childAdder Game.childRemover this address None
        | Some childRcd ->
            let optChild = getOptChild Game.optChildFinder this address
            match optChild with
            | None -> failwith "Cannot change a non-existent screen."
            | Some child ->
                let child2 = set childRcd child lens
                setChild Game.childAdder Game.childRemover this address child2

    static member gameRcd =
        { Get = fun this ->
            match this with
            | Game gameRcd -> gameRcd
          Set = fun gameRcd this ->
            match this with
            | Game _ -> Game gameRcd }

    static member screen address =
        { Get = fun this -> Game.getChildWithLens this address Lens.id
          Set = fun screenRcd this -> Game.setChildWithLens screenRcd this address Lens.id }

    static member screenRcd address =
        { Get = fun this -> Game.getChildWithLens this address Screen.screenRcd
          Set = fun screenRcd this -> Game.setChildWithLens screenRcd this address Screen.screenRcd }

    static member optScreenRcd address =
        { Get = fun this -> Game.getOptChildWithLens this address Screen.screenRcd
          Set = fun optScreenRcd this -> Game.setOptChildWithLens optScreenRcd this address Screen.screenRcd }
    
    static member entityRcd address = Game.screen [List.head address] >>| Screen.entityRcd (List.tail address)
    static member optEntityRcd address = Game.screen [List.head address] >>| Screen.optEntityRcd (List.tail address)

    static member gui address = Game.screen [List.head address] >>| Screen.gui (List.tail address)
    static member optGui address = Game.screen [List.head address] >>| Screen.optGui (List.tail address)

    static member button address = Game.screen [List.head address] >>| Screen.button (List.tail address)
    static member optButton address = Game.screen [List.head address] >>| Screen.optButton (List.tail address)

    static member label address = Game.screen [List.head address] >>| Screen.label (List.tail address)
    static member optLabel address = Game.screen [List.head address] >>| Screen.optLabel (List.tail address)

    static member textBox address = Game.screen [List.head address] >>| Screen.textBox (List.tail address)
    static member optTextBox address = Game.screen [List.head address] >>| Screen.optTextBox (List.tail address)

    static member toggle address = Game.screen [List.head address] >>| Screen.toggle (List.tail address)
    static member optToggle address = Game.screen [List.head address] >>| Screen.optToggle (List.tail address)

    static member feeler address = Game.screen [List.head address] >>| Screen.feeler (List.tail address)
    static member optFeeler address = Game.screen [List.head address] >>| Screen.optFeeler (List.tail address)

    static member actor address = Game.screen [List.head address] >>| Screen.actor (List.tail address)
    static member optActor address = Game.screen [List.head address] >>| Screen.optActor (List.tail address)

    static member block address = Game.screen [List.head address] >>| Screen.block (List.tail address)
    static member optBlock address = Game.screen [List.head address] >>| Screen.optBlock (List.tail address)

    static member avatar address = Game.screen [List.head address] >>| Screen.avatar (List.tail address)
    static member optAvatar address = Game.screen [List.head address] >>| Screen.optAvatar (List.tail address)

    static member tileMap address = Game.screen [List.head address] >>| Screen.tileMap (List.tail address)
    static member optTileMap address = Game.screen [List.head address] >>| Screen.optTileMap (List.tail address)
    
    static member groupRcd address = Game.screen [List.head address] >>| Screen.groupRcd (List.tail address)
    static member optGroupRcd address = Game.screen [List.head address] >>| Screen.optGroupRcd (List.tail address)