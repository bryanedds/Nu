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

let screenLens =
    { Get = fun this ->
        match this with
        | Screen screen -> screen
      Set = fun screen this ->
        match this with
        | Screen _ -> Screen screen }
       
let private screenModelOptChildModelFinder addressHead this =
    let screen = get this screenLens
    Map.tryFind addressHead screen.GroupModels

let private screenModelChildModelAdder addressHead this (child : GroupModel) =
    let screen = get this screenLens
    let screen2 = { screen with GroupModels = Map.add addressHead child screen.GroupModels }
    set screen2 this screenLens

let private screenModelChildModelRemover addressHead this =
    let screen = get this screenLens
    let screen2 = { screen with GroupModels = Map.remove addressHead screen.GroupModels }
    set screen2 this screenLens

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

let groupModelsLens =
    { Get = fun this -> (get this screenLens).GroupModels
      Set = fun groupModels this -> set { (get this screenLens) with GroupModels = groupModels } this screenLens }

let groupModelLens address =
    { Get = fun this -> Option.get <| screenModelOptChildModelFinder (List.head address) this
      Set = fun group this -> screenModelChildModelAdder (List.head address) this group }

let optGroupModelLens address =
    { Get = fun this -> screenModelOptChildModelFinder (List.head address) this
      Set = fun optGroup this -> match optGroup with None -> screenModelChildModelRemover (List.head address) this | Some entity -> screenModelChildModelAdder (List.head address) this entity }

let screenModelGroupLens address =
    { Get = fun this -> screenModelGetChildWithLens this address groupLens
      Set = fun group this -> screenModelSetChildWithLens group this address groupLens }

let screenModelOptGroupLens address =
    { Get = fun this -> screenModelGetOptChildWithLens this address groupLens
      Set = fun optGroup this -> screenModelSetOptChildWithLens optGroup this address groupLens }

let screenModelEntityModelLens address = groupModelLens [List.head address] >>| entityModelLens (List.tail address)
let screenModelOptEntityModelLens address = groupModelLens [List.head address] >>| optEntityModelLens (List.tail address)

let screenModelEntityLens address = groupModelLens [List.head address] >>| groupModelEntityLens (List.tail address)
let screenModelOptEntityLens address = groupModelLens [List.head address] >>| groupModelOptEntityLens (List.tail address)

let screenModelGuiLens address = groupModelLens [List.head address] >>| groupModelGuiLens (List.tail address)
let screenModelOptGuiLens address = groupModelLens [List.head address] >>| groupModelOptGuiLens (List.tail address)

let screenModelButtonLens address = groupModelLens [List.head address] >>| groupModelButtonLens (List.tail address)
let screenModelOptButtonLens address = groupModelLens [List.head address] >>| groupModelOptButtonLens (List.tail address)

let screenModelLabelLens address = groupModelLens [List.head address] >>| groupModelLabelLens (List.tail address)
let screenModelOptLabelLens address = groupModelLens [List.head address] >>| groupModelOptLabelLens (List.tail address)

let screenModeltextBoxLens address = groupModelLens [List.head address] >>| groupModelTextBoxLens (List.tail address)
let screenModelOptTextBoxLens address = groupModelLens [List.head address] >>| groupModelOptTextBoxLens (List.tail address)

let screenModelToggleLens address = groupModelLens [List.head address] >>| groupModelToggleLens (List.tail address)
let screenModelOptToggleLens address = groupModelLens [List.head address] >>| groupModelOptToggleLens (List.tail address)

let screenModelFeelerLens address = groupModelLens [List.head address] >>| groupModelFeelerLens (List.tail address)
let screenModelOptFeelerLens address = groupModelLens [List.head address] >>| groupModelOptFeelerLens (List.tail address)

let screenModelActorLens address = groupModelLens [List.head address] >>| groupModelActorLens (List.tail address)
let screenModelOptActorLens address = groupModelLens [List.head address] >>| groupModelOptActorLens (List.tail address)

let screenModelBlockLens address = groupModelLens [List.head address] >>| groupModelBlockLens (List.tail address)
let screenModelOptBlockLens address = groupModelLens [List.head address] >>| groupModelOptBlockLens (List.tail address)

let screenModelAvatarLens address = groupModelLens [List.head address] >>| groupModelAvatarLens (List.tail address)
let screenModelOptAvatarLens address = groupModelLens [List.head address] >>| groupModelOptAvatarLens (List.tail address)

let screenModelTileMapLens address = groupModelLens [List.head address] >>| groupModelTileMapLens (List.tail address)
let screenModelOptTileMapLens address = groupModelLens [List.head address] >>| groupModelOptTileMapLens (List.tail address)