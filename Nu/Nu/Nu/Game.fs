module Nu.Game
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DomainModel
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

type [<StructuralEquality; NoComparison; CLIMutable>] Game =
    { Id : Id
      ScreenModels : Map<Lun, ScreenModel>
      OptSelectedScreenAddress : Address option }
        
type [<StructuralEquality; NoComparison>] GameModel =
    | Game of Game
       
    static member private optChildModelFinder addressHead this =
        let game = get this GameModel.game
        Map.tryFind addressHead game.ScreenModels
    
    static member private childModelAdder addressHead this (child : ScreenModel) =
        let game = get this GameModel.game
        let game2 = { game with ScreenModels = Map.add addressHead child game.ScreenModels }
        set game2 this GameModel.game
    
    static member private childModelRemover addressHead this =
        let game = get this GameModel.game
        let game2 = { game with ScreenModels = Map.remove addressHead game.ScreenModels }
        set game2 this GameModel.game

    static member private getChildWithLens this address lens =
        get (getChild GameModel.optChildModelFinder this address) lens

    static member private setChildWithLens child this address lens =
        let screen = getChild GameModel.optChildModelFinder this address
        let screen2 = set child screen lens
        setChild GameModel.childModelAdder GameModel.childModelRemover this address screen2

    static member private getOptChildWithLens this address lens =
        let optChild = getOptChild GameModel.optChildModelFinder this address
        match optChild with
        | None -> None
        | Some child -> Some (get child lens)

    static member private setOptChildWithLens optChild this address lens =
        match optChild with
        | None -> setOptChild GameModel.childModelAdder GameModel.childModelRemover this address None
        | Some child ->
            let optChildModel = getOptChild GameModel.optChildModelFinder this address
            match optChildModel with
            | None -> failwith "Cannot change a non-existent screen."
            | Some childModel ->
                let childModel2 = set child childModel lens
                setChild GameModel.childModelAdder GameModel.childModelRemover this address childModel2

    static member game =
        { Get = fun this ->
            match this with
            | Game game -> game
          Set = fun game this ->
            match this with
            | Game _ -> Game game }
    
    static member optActiveScreenAddress =
        { Get = fun this -> (get this GameModel.game).OptSelectedScreenAddress
          Set = fun optSelectedScreenAddress this -> set { (get this GameModel.game) with OptSelectedScreenAddress = optSelectedScreenAddress } this  World.game }

    static member screenModel address =
        { Get = fun this -> GameModel.getChildWithLens this address Lens.id
          Set = fun screen this -> GameModel.setChildWithLens screen this address Lens.id }

    static member screen address =
        { Get = fun this -> GameModel.getChildWithLens this address ScreenModel.screen
          Set = fun screen this -> GameModel.setChildWithLens screen this address ScreenModel.screen }

    static member optScreen address =
        { Get = fun this -> GameModel.getOptChildWithLens this address ScreenModel.screen
          Set = fun optScreen this -> GameModel.setOptChildWithLens optScreen this address ScreenModel.screen }
    
    static member entity address = GameModel.screenModel [List.head address] >>| ScreenModel.entity (List.tail address)
    static member optEntity address = GameModel.screenModel [List.head address] >>| ScreenModel.optEntity (List.tail address)

    static member gui address = GameModel.screenModel [List.head address] >>| ScreenModel.gui (List.tail address)
    static member optGui address = GameModel.screenModel [List.head address] >>| ScreenModel.optGui (List.tail address)

    static member button address = GameModel.screenModel [List.head address] >>| ScreenModel.button (List.tail address)
    static member optButton address = GameModel.screenModel [List.head address] >>| ScreenModel.optButton (List.tail address)

    static member label address = GameModel.screenModel [List.head address] >>| ScreenModel.label (List.tail address)
    static member optLabel address = GameModel.screenModel [List.head address] >>| ScreenModel.optLabel (List.tail address)

    static member textBox address = GameModel.screenModel [List.head address] >>| ScreenModel.textBox (List.tail address)
    static member optTextBox address = GameModel.screenModel [List.head address] >>| ScreenModel.optTextBox (List.tail address)

    static member toggle address = GameModel.screenModel [List.head address] >>| ScreenModel.toggle (List.tail address)
    static member optToggle address = GameModel.screenModel [List.head address] >>| ScreenModel.optToggle (List.tail address)

    static member feeler address = GameModel.screenModel [List.head address] >>| ScreenModel.feeler (List.tail address)
    static member optFeeler address = GameModel.screenModel [List.head address] >>| ScreenModel.optFeeler (List.tail address)

    static member actor address = GameModel.screenModel [List.head address] >>| ScreenModel.actor (List.tail address)
    static member optActor address = GameModel.screenModel [List.head address] >>| ScreenModel.optActor (List.tail address)

    static member block address = GameModel.screenModel [List.head address] >>| ScreenModel.block (List.tail address)
    static member optBlock address = GameModel.screenModel [List.head address] >>| ScreenModel.optBlock (List.tail address)

    static member avatar address = GameModel.screenModel [List.head address] >>| ScreenModel.avatar (List.tail address)
    static member optAvatar address = GameModel.screenModel [List.head address] >>| ScreenModel.optAvatar (List.tail address)

    static member tileMap address = GameModel.screenModel [List.head address] >>| ScreenModel.tileMap (List.tail address)
    static member optTileMap address = GameModel.screenModel [List.head address] >>| ScreenModel.optTileMap (List.tail address)
    
    static member group address = GameModel.screenModel [List.head address] >>| ScreenModel.group (List.tail address)
    static member optGroup address = GameModel.screenModel [List.head address] >>| ScreenModel.optGroup (List.tail address)