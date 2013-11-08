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
      OptSelectedScreenModelAddress : Address option }
        
type [<StructuralEquality; NoComparison>] GameModel =
    | Game of Game

let gameModelGame =
    { Get = fun this ->
        match this with
        | Game game -> game
      Set = fun game this ->
        match this with
        | Game _ -> Game game }
       
let private gameModelOptChildModelFinder addressHead this =
    let game = get this gameModelGame
    Map.tryFind addressHead game.ScreenModels

let private gameModelChildModelAdder addressHead this (child : ScreenModel) =
    let game = get this gameModelGame
    let game2 = { game with ScreenModels = Map.add addressHead child game.ScreenModels }
    set game2 this gameModelGame

let private gameModelChildModelRemover addressHead this =
    let game = get this gameModelGame
    let game2 = { game with ScreenModels = Map.remove addressHead game.ScreenModels }
    set game2 this gameModelGame

let private gameModelGetChildWithLens this address lens =
    get (getChild gameModelOptChildModelFinder this address) lens

let private gameModelSetChildWithLens child this address lens =
    let screen = getChild gameModelOptChildModelFinder this address
    let screen2 = set child screen lens
    setChild gameModelChildModelAdder gameModelChildModelRemover this address screen2

let private gameModelGetOptChildWithLens this address lens =
    let optChild = getOptChild gameModelOptChildModelFinder this address
    match optChild with
    | None -> None
    | Some child -> Some (get child lens)

let private gameModelSetOptChildWithLens optChild this address lens =
    match optChild with
    | None -> setOptChild gameModelChildModelAdder gameModelChildModelRemover this address None
    | Some child ->
        let optChildModel = getOptChild gameModelOptChildModelFinder this address
        match optChildModel with
        | None -> failwith "Cannot change a non-existent screen."
        | Some childModel ->
            let childModel2 = set child childModel lens
            setChild gameModelChildModelAdder gameModelChildModelRemover this address childModel2

let gameModelScreenModels =
    { Get = fun this -> (get this gameModelGame).ScreenModels
      Set = fun screenModels this -> set { (get this gameModelGame) with ScreenModels = screenModels } this gameModelGame }

let gameModelOptSelectedScreenModelAddress =
    { Get = fun this -> (get this gameModelGame).OptSelectedScreenModelAddress
      Set = fun optSelectedScreenModelAddress this -> set { (get this gameModelGame) with OptSelectedScreenModelAddress = optSelectedScreenModelAddress } this gameModelGame}

let gameModelScreenModel address =
    { Get = fun this -> Option.get <| gameModelOptChildModelFinder (List.head address) this
      Set = fun screen this -> gameModelChildModelAdder (List.head address) this screen }

let gameModelOptScreenModel address =
    { Get = fun this -> gameModelOptChildModelFinder (List.head address) this
      Set = fun optScreen this -> match optScreen with None -> gameModelChildModelRemover (List.head address) this | Some entity -> gameModelChildModelAdder (List.head address) this entity }

let gameModelScreen address =
    { Get = fun this -> gameModelGetChildWithLens this address screenModelscreen
      Set = fun screen this -> gameModelSetChildWithLens screen this address screenModelscreen }

let gameModelOptScreen address =
    { Get = fun this -> gameModelGetOptChildWithLens this address screenModelscreen
      Set = fun optScreen this -> gameModelSetOptChildWithLens optScreen this address screenModelscreen }

let gameModelGroupModel address = gameModelScreenModel [List.head address] >>| screenModelGroupModel (List.tail address)
let gameModelOptGroupModel address = gameModelScreenModel [List.head address] >>| screenModelOptGroupModel (List.tail address)

let gameModelGroup address = gameModelScreenModel [List.head address] >>| screenModelGroup (List.tail address)
let gameModelOptGroup address = gameModelScreenModel [List.head address] >>| screenModelOptGroup (List.tail address)

let gameModelEntityModel address = gameModelScreenModel [List.head address] >>| screenModelEntityModel (List.tail address)
let gameModelOptEntityModel address = gameModelScreenModel [List.head address] >>| screenModelOptEntityModel (List.tail address)

let gameModelEntity address = gameModelScreenModel [List.head address] >>| screenModelEntity (List.tail address)
let gameModelOptEntity address = gameModelScreenModel [List.head address] >>| screenModelOptEntity (List.tail address)

let gameModelGui address = gameModelScreenModel [List.head address] >>| screenModelGui (List.tail address)
let gameModelOptGui address = gameModelScreenModel [List.head address] >>| screenModelOptGui (List.tail address)

let gameModelButton address = gameModelScreenModel [List.head address] >>| screenModelButton (List.tail address)
let gameModelOptButton address = gameModelScreenModel [List.head address] >>| screenModelOptButton (List.tail address)

let gameModelLabel address = gameModelScreenModel [List.head address] >>| screenModelLabel (List.tail address)
let gameModelOptLabel address = gameModelScreenModel [List.head address] >>| screenModelOptLabel (List.tail address)

let gameModelTextBox address = gameModelScreenModel [List.head address] >>| screenModeltextBox (List.tail address)
let gameModelOptTextBox address = gameModelScreenModel [List.head address] >>| screenModelOptTextBox (List.tail address)

let gameModelToggle address = gameModelScreenModel [List.head address] >>| screenModelToggle (List.tail address)
let gameModelOptToggle address = gameModelScreenModel [List.head address] >>| screenModelOptToggle (List.tail address)

let gameModelFeeler address = gameModelScreenModel [List.head address] >>| screenModelFeeler (List.tail address)
let gameModelOptFeeler address = gameModelScreenModel [List.head address] >>| screenModelOptFeeler (List.tail address)

let gameModelActor address = gameModelScreenModel [List.head address] >>| screenModelActor (List.tail address)
let gameModelOptActor address = gameModelScreenModel [List.head address] >>| screenModelOptActor (List.tail address)

let gameModelBlock address = gameModelScreenModel [List.head address] >>| screenModelBlock (List.tail address)
let gameModelOptBlock address = gameModelScreenModel [List.head address] >>| screenModelOptBlock (List.tail address)

let gameModelAvatar address = gameModelScreenModel [List.head address] >>| screenModelAvatar (List.tail address)
let gameModelOptAvatar address = gameModelScreenModel [List.head address] >>| screenModelOptAvatar (List.tail address)

let gameModelTileMap address = gameModelScreenModel [List.head address] >>| screenModelTileMap (List.tail address)
let gameModelOptTileMap address = gameModelScreenModel [List.head address] >>| screenModelOptTileMap (List.tail address)