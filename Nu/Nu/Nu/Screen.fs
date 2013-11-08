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

let screenModelscreen =
    { Get = fun this ->
        match this with
        | Screen screen -> screen
      Set = fun screen this ->
        match this with
        | Screen _ -> Screen screen }
       
let private screenModelOptChildModelFinder addressHead this =
    let screen = get this screenModelscreen
    Map.tryFind addressHead screen.GroupModels

let private screenModelChildModelAdder addressHead this (child : GroupModel) =
    let screen = get this screenModelscreen
    let screen2 = { screen with GroupModels = Map.add addressHead child screen.GroupModels }
    set screen2 this screenModelscreen

let private screenModelChildModelRemover addressHead this =
    let screen = get this screenModelscreen
    let screen2 = { screen with GroupModels = Map.remove addressHead screen.GroupModels }
    set screen2 this screenModelscreen

let private screenModelGetChildWithLens this address lens =
    get (getChild screenModelOptChildModelFinder this address) lens

let private screenModelSetChildWithLens child this address lens =
    let group = getChild screenModelOptChildModelFinder this address
    let group2 = set child group lens
    setChild screenModelChildModelAdder screenModelChildModelRemover this address group2

let private screenModelGetOptChildWithLens this address lens =
    let optChild = getOptChild screenModelOptChildModelFinder this address
    match optChild with
    | None -> None
    | Some child -> Some (get child lens)

let private screenModelSetOptChildWithLens optChild this address lens =
    match optChild with
    | None -> setOptChild screenModelChildModelAdder screenModelChildModelRemover this address None
    | Some child ->
        let optChildModel = getOptChild screenModelOptChildModelFinder this address
        match optChildModel with
        | None -> failwith "Cannot change a non-existent group."
        | Some childModel ->
            let childModel2 = set child childModel lens
            setChild screenModelChildModelAdder screenModelChildModelRemover this address childModel2

let screenModelGroupModels =
    { Get = fun this -> (get this screenModelscreen).GroupModels
      Set = fun groupModels this -> set { (get this screenModelscreen) with GroupModels = groupModels } this screenModelscreen }

let screenModelGroupModel address =
    { Get = fun this -> Option.get <| screenModelOptChildModelFinder (List.head address) this
      Set = fun group this -> screenModelChildModelAdder (List.head address) this group }

let screenModelOptGroupModel address =
    { Get = fun this -> screenModelOptChildModelFinder (List.head address) this
      Set = fun optGroup this -> match optGroup with None -> screenModelChildModelRemover (List.head address) this | Some entity -> screenModelChildModelAdder (List.head address) this entity }

let screenModelGroup address =
    { Get = fun this -> screenModelGetChildWithLens this address groupModelGroup
      Set = fun group this -> screenModelSetChildWithLens group this address groupModelGroup }

let screenModelOptGroup address =
    { Get = fun this -> screenModelGetOptChildWithLens this address groupModelGroup
      Set = fun optGroup this -> screenModelSetOptChildWithLens optGroup this address groupModelGroup }

let screenModelEntityModel address = screenModelGroupModel [List.head address] >>| groupModelEntityModel (List.tail address)
let screenModelOptEntityModel address = screenModelGroupModel [List.head address] >>| groupModelOptEntityModel (List.tail address)

let screenModelEntity address = screenModelGroupModel [List.head address] >>| groupModelEntity (List.tail address)
let screenModelOptEntity address = screenModelGroupModel [List.head address] >>| groupModelOptEntity (List.tail address)

let screenModelGui address = screenModelGroupModel [List.head address] >>| groupModelGui (List.tail address)
let screenModelOptGui address = screenModelGroupModel [List.head address] >>| groupModelOptGui (List.tail address)

let screenModelButton address = screenModelGroupModel [List.head address] >>| groupModelButton (List.tail address)
let screenModelOptButton address = screenModelGroupModel [List.head address] >>| groupModelOptButton (List.tail address)

let screenModelLabel address = screenModelGroupModel [List.head address] >>| groupModelLabel (List.tail address)
let screenModelOptLabel address = screenModelGroupModel [List.head address] >>| groupModelOptLabel (List.tail address)

let screenModeltextBox address = screenModelGroupModel [List.head address] >>| groupModelTextBox (List.tail address)
let screenModelOptTextBox address = screenModelGroupModel [List.head address] >>| groupModelOptTextBox (List.tail address)

let screenModelToggle address = screenModelGroupModel [List.head address] >>| groupModelToggle (List.tail address)
let screenModelOptToggle address = screenModelGroupModel [List.head address] >>| groupModelOptToggle (List.tail address)

let screenModelFeeler address = screenModelGroupModel [List.head address] >>| groupModelFeeler (List.tail address)
let screenModelOptFeeler address = screenModelGroupModel [List.head address] >>| groupModelOptFeeler (List.tail address)

let screenModelActor address = screenModelGroupModel [List.head address] >>| groupModelActor (List.tail address)
let screenModelOptActor address = screenModelGroupModel [List.head address] >>| groupModelOptActor (List.tail address)

let screenModelBlock address = screenModelGroupModel [List.head address] >>| groupModelBlock (List.tail address)
let screenModelOptBlock address = screenModelGroupModel [List.head address] >>| groupModelOptBlock (List.tail address)

let screenModelAvatar address = screenModelGroupModel [List.head address] >>| groupModelAvatar (List.tail address)
let screenModelOptAvatar address = screenModelGroupModel [List.head address] >>| groupModelOptAvatar (List.tail address)

let screenModelTileMap address = screenModelGroupModel [List.head address] >>| groupModelTileMap (List.tail address)
let screenModelOptTileMap address = screenModelGroupModel [List.head address] >>| groupModelOptTileMap (List.tail address)