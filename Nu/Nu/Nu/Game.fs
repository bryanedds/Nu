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

let gameLens =
    { Get = fun this ->
        match this with
        | Game game -> game
      Set = fun game this ->
        match this with
        | Game _ -> Game game }
       
let private gameModelOptChildModelFinder addressHead this =
    let game = get this gameLens
    Map.tryFind addressHead game.ScreenModels

let private gameModelChildModelAdder addressHead this (child : ScreenModel) =
    let game = get this gameLens
    let game2 = { game with ScreenModels = Map.add addressHead child game.ScreenModels }
    set game2 this gameLens

let private gameModelChildModelRemover addressHead this =
    let game = get this gameLens
    let game2 = { game with ScreenModels = Map.remove addressHead game.ScreenModels }
    set game2 this gameLens

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

let screenModelsLens =
    { Get = fun this -> (get this gameLens).ScreenModels
      Set = fun screenModels this -> set { (get this gameLens) with ScreenModels = screenModels } this gameLens }

let optSelectedScreenModelAddressLens =
    { Get = fun this -> (get this gameLens).OptSelectedScreenModelAddress
      Set = fun optSelectedScreenModelAddress this -> set { (get this gameLens) with OptSelectedScreenModelAddress = optSelectedScreenModelAddress } this gameLens}

let screenModelLens address =
    { Get = fun this -> Option.get <| gameModelOptChildModelFinder (List.head address) this
      Set = fun screen this -> gameModelChildModelAdder (List.head address) this screen }

let optScreenModelLens address =
    { Get = fun this -> gameModelOptChildModelFinder (List.head address) this
      Set = fun optScreen this -> match optScreen with None -> gameModelChildModelRemover (List.head address) this | Some entity -> gameModelChildModelAdder (List.head address) this entity }

let gameModelScreenLens address =
    { Get = fun this -> gameModelGetChildWithLens this address screenLens
      Set = fun screen this -> gameModelSetChildWithLens screen this address screenLens }

let gameModelOptScreenLens address =
    { Get = fun this -> gameModelGetOptChildWithLens this address screenLens
      Set = fun optScreen this -> gameModelSetOptChildWithLens optScreen this address screenLens }

let gameModelGroupModelLens address = screenModelLens [List.head address] >>| groupModelLens (List.tail address)
let gameModelOptGroupModelLens address = screenModelLens [List.head address] >>| optGroupModelLens (List.tail address)

let gameModelGroupLens address = screenModelLens [List.head address] >>| screenModelGroupLens (List.tail address)
let gameModelOptGroupLens address = screenModelLens [List.head address] >>| screenModelOptGroupLens (List.tail address)

let gameModelEntityModelLens address = screenModelLens [List.head address] >>| screenModelEntityModelLens (List.tail address)
let gameModelOptEntityModelLens address = screenModelLens [List.head address] >>| screenModelOptEntityModelLens (List.tail address)

let gameModelEntityLens address = screenModelLens [List.head address] >>| screenModelEntityLens (List.tail address)
let gameModelOptEntityLens address = screenModelLens [List.head address] >>| screenModelOptEntityLens (List.tail address)

let gameModelGuiLens address = screenModelLens [List.head address] >>| screenModelGuiLens (List.tail address)
let gameModelOptGuiLens address = screenModelLens [List.head address] >>| screenModelOptGuiLens (List.tail address)

let gameModelButtonLens address = screenModelLens [List.head address] >>| screenModelButtonLens (List.tail address)
let gameModelOptButtonLens address = screenModelLens [List.head address] >>| screenModelOptButtonLens (List.tail address)

let gameModelLabelLens address = screenModelLens [List.head address] >>| screenModelLabelLens (List.tail address)
let gameModelOptLabelLens address = screenModelLens [List.head address] >>| screenModelOptLabelLens (List.tail address)

let gameModelTextBoxLens address = screenModelLens [List.head address] >>| screenModeltextBoxLens (List.tail address)
let gameModelOptTextBoxLens address = screenModelLens [List.head address] >>| screenModelOptTextBoxLens (List.tail address)

let gameModelToggleLens address = screenModelLens [List.head address] >>| screenModelToggleLens (List.tail address)
let gameModelOptToggleLens address = screenModelLens [List.head address] >>| screenModelOptToggleLens (List.tail address)

let gameModelFeelerLens address = screenModelLens [List.head address] >>| screenModelFeelerLens (List.tail address)
let gameModelOptFeelerLens address = screenModelLens [List.head address] >>| screenModelOptFeelerLens (List.tail address)

let gameModelActorLens address = screenModelLens [List.head address] >>| screenModelActorLens (List.tail address)
let gameModelOptActorLens address = screenModelLens [List.head address] >>| screenModelOptActorLens (List.tail address)

let gameModelBlockLens address = screenModelLens [List.head address] >>| screenModelBlockLens (List.tail address)
let gameModelOptBlockLens address = screenModelLens [List.head address] >>| screenModelOptBlockLens (List.tail address)

let gameModelAvatarLens address = screenModelLens [List.head address] >>| screenModelAvatarLens (List.tail address)
let gameModelOptAvatarLens address = screenModelLens [List.head address] >>| screenModelOptAvatarLens (List.tail address)

let gameModelTileMapLens address = screenModelLens [List.head address] >>| screenModelTileMapLens (List.tail address)
let gameModelOptTileMapLens address = screenModelLens [List.head address] >>| screenModelOptTileMapLens (List.tail address)