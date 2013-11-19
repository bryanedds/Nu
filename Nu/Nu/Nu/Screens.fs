namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DomainModel
open Nu.Entities
open Nu.Groups

type [<StructuralEquality; NoComparison>] TransitionType =
    | Incoming
    | Outgoing

type [<StructuralEquality; NoComparison; CLIMutable>] Transition =
    { Id : Id
      Lifetime : int
      Ticks : int
      Type : TransitionType }

type [<StructuralEquality; NoComparison; CLIMutable>] Dissolve =
    { Transition : Transition
      Sprite : Sprite }

type [<StructuralEquality; NoComparison>] TransitionModel =
    | Transition of Transition
    | Dissolve of Dissolve

type [<StructuralEquality; NoComparison>] ScreenState =
    | IncomingState
    | OutgoingState
    | IdlingState

type [<StructuralEquality; NoComparison; CLIMutable>] Screen =
    { Id : Id
      State : ScreenState
      IncomingModel : TransitionModel
      OutgoingModel : TransitionModel
      GroupModels : Map<Lun, GroupModel> }

type [<StructuralEquality; NoComparison>] ScreenModel =
    | Screen of Screen

module Screens =

    let transitionLens =
        { Get = fun this ->
            match this with
            | Transition transition -> transition
            | Dissolve dissolve -> dissolve.Transition
          Set = fun transition this ->
            match this with
            | Transition _ -> Transition transition
            | Dissolve dissolve -> Dissolve { dissolve with Transition = transition }}

    let transitionIdLens =
        { Get = fun this -> (get this transitionLens).Id
          Set = fun value this -> set { get this transitionLens with Id = value } this transitionLens }

    let transitionLifetimeLens =
        { Get = fun this -> (get this transitionLens).Lifetime
          Set = fun value this -> set { get this transitionLens with Lifetime = value } this transitionLens }

    let transitionTicksLens =
        { Get = fun this -> (get this transitionLens).Ticks
          Set = fun value this -> set { get this transitionLens with Ticks = value } this transitionLens }

    let transitionTypeLens =
        { Get = fun this -> (get this transitionLens).Type
          Set = fun value this -> set { get this transitionLens with Type = value } this transitionLens }

    let screenLens =
        { Get = fun this ->
            match this with
            | Screen screen -> screen
          Set = fun screen this ->
            match this with
            | Screen _ -> Screen screen }

    let screenIdLens =
        { Get = fun this -> (get this screenLens).Id
          Set = fun value this -> set { get this screenLens with Id = value } this screenLens }

    let screenStateLens =
        { Get = fun this -> (get this screenLens).State
          Set = fun value this -> set { get this screenLens with State = value } this screenLens }

    let screenIncomingModelLens =
        { Get = fun this -> (get this screenLens).IncomingModel
          Set = fun value this -> set { get this screenLens with IncomingModel = value } this screenLens }

    let screenOutgoingModelLens =
        { Get = fun this -> (get this screenLens).OutgoingModel
          Set = fun value this -> set { get this screenLens with OutgoingModel = value } this screenLens }

    let screenGroupModelsLens =
        { Get = fun this -> (get this screenLens).GroupModels
          Set = fun value this -> set { get this screenLens with GroupModels = value } this screenLens }
       
    let private screenModelOptChildModelFinder addressHead this =
        let screen = get this screenLens
        Map.tryFind addressHead screen.GroupModels

    let private screenModelChildModelAdder addressHead this (child : GroupModel) =
        let screen = get this screenLens
        let screen' = { screen with GroupModels = Map.add addressHead child screen.GroupModels }
        set screen' this screenLens

    let private screenModelChildModelRemover addressHead this =
        let screen = get this screenLens
        let screen' = { screen with GroupModels = Map.remove addressHead screen.GroupModels }
        set screen' this screenLens

    let private screenModelGetChildWithLens this address lens =
        get (getChild screenModelOptChildModelFinder this address) lens

    let private screenModelSetChildWithLens child this address lens =
        let group = getChild screenModelOptChildModelFinder this address
        let group' = set child group lens
        setChild screenModelChildModelAdder screenModelChildModelRemover this address group'

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
                let childModel' = set child childModel lens
                setChild screenModelChildModelAdder screenModelChildModelRemover this address childModel'

    let incomingModelLens =
        { Get = fun this -> (get this screenLens).IncomingModel
          Set = fun incoming this -> set { (get this screenLens) with IncomingModel = incoming } this screenLens }

    let outgoingModelLens =
        { Get = fun this -> (get this screenLens).OutgoingModel
          Set = fun outgoing this -> set { (get this screenLens) with OutgoingModel = outgoing } this screenLens }

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
    
    let makeDissolveSprite () =
        { SpriteAssetName = Lun.make "Image8"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

    let makeDefaultTransition transitionType =
        { Id = getNuId ()
          Lifetime = 0
          Ticks = 0
          Type = transitionType }

    let makeDissolveTransition lifetime transitionType =
        { Transition = { makeDefaultTransition transitionType with Lifetime = lifetime }; Sprite = makeDissolveSprite () }

    let makeDefaultScreen () =
        { Id = getNuId ()
          State = IncomingState
          IncomingModel = Transition <| makeDefaultTransition Incoming
          OutgoingModel = Transition <| makeDefaultTransition Outgoing
          GroupModels = Map.empty }

    let makeDissolveScreen incomingTime outgoingTime =
        let incomingDissolve = Dissolve <| makeDissolveTransition incomingTime Incoming
        let outgoingDissolve = Dissolve <| makeDissolveTransition outgoingTime Outgoing
        { makeDefaultScreen () with IncomingModel = incomingDissolve; OutgoingModel = outgoingDissolve }