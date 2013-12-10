namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu
open Nu.Core
open Nu.DomainModel
module Screens =

    let transitionLens =
        { Get = fun transitionModel ->
            match transitionModel with
            | Transition transition -> transition
            | Dissolve dissolve -> dissolve.Transition
          Set = fun transition transitionModel ->
            match transitionModel with
            | Transition _ -> Transition transition
            | Dissolve dissolve -> Dissolve { dissolve with Transition = transition }}

    let transitionIdLens =
        { Get = fun transitionModel -> (get transitionModel transitionLens).Id
          Set = fun value transitionModel -> set { get transitionModel transitionLens with Id = value } transitionModel transitionLens }

    let transitionLifetimeLens =
        { Get = fun transitionModel -> (get transitionModel transitionLens).Lifetime
          Set = fun value transitionModel -> set { get transitionModel transitionLens with Lifetime = value } transitionModel transitionLens }

    let transitionTicksLens =
        { Get = fun transitionModel -> (get transitionModel transitionLens).Ticks
          Set = fun value transitionModel -> set { get transitionModel transitionLens with Ticks = value } transitionModel transitionLens }

    let transitionTypeLens =
        { Get = fun transitionModel -> (get transitionModel transitionLens).Type
          Set = fun value transitionModel -> set { get transitionModel transitionLens with Type = value } transitionModel transitionLens }

    let screenLens =
        { Get = fun screenModel ->
            match screenModel with
            | Screen screen -> screen
            | OmniBattleScreen omniBattleScreen -> omniBattleScreen.Screen
          Set = fun screen screenModel ->
            match screenModel with
            | Screen _ -> Screen screen
            | OmniBattleScreen omniBattleScreen -> OmniBattleScreen { omniBattleScreen with Screen = screen }}

    let screenIdLens =
        { Get = fun screenModel -> (get screenModel screenLens).Id
          Set = fun value screenModel -> set { get screenModel screenLens with Id = value } screenModel screenLens }

    let screenStateLens =
        { Get = fun screenModel -> (get screenModel screenLens).State
          Set = fun value screenModel -> set { get screenModel screenLens with State = value } screenModel screenLens }

    let screenIncomingModelLens =
        { Get = fun screenModel -> (get screenModel screenLens).IncomingModel
          Set = fun value screenModel -> set { get screenModel screenLens with IncomingModel = value } screenModel screenLens }

    let screenOutgoingModelLens =
        { Get = fun screenModel -> (get screenModel screenLens).OutgoingModel
          Set = fun value screenModel -> set { get screenModel screenLens with OutgoingModel = value } screenModel screenLens }

    let incomingModelLens =
        { Get = fun screenModel -> (get screenModel screenLens).IncomingModel
          Set = fun incoming screenModel -> set { (get screenModel screenLens) with IncomingModel = incoming } screenModel screenLens }

    let outgoingModelLens =
        { Get = fun screenModel -> (get screenModel screenLens).OutgoingModel
          Set = fun outgoing screenModel -> set { (get screenModel screenLens) with OutgoingModel = outgoing } screenModel screenLens }
       
    let worldOptScreenModelFinder (address : Address)  world =
        Map.tryFind address.[0] world.ScreenModels

    let worldScreenModelAdder (address : Address) world (child : ScreenModel) =
        { world with ScreenModels = Map.add address.[0] child world.ScreenModels }

    let worldScreenModelRemover (address : Address)  world =
        { world with ScreenModels = Map.remove address.[0] world.ScreenModels }

    let getWorldScreenModelWithLens address world lens =
        get (getChild worldOptScreenModelFinder address world) lens

    let setWorldScreenModelWithLens child address world lens =
        let screen = getChild worldOptScreenModelFinder address world
        let screen' = set child screen lens
        setChild worldScreenModelAdder worldScreenModelRemover address world screen'

    let getWorldOptScreenModelWithLens address world lens =
        let optChild = getOptChild worldOptScreenModelFinder address world
        match optChild with None -> None | Some child -> Some (get child lens)

    let setWorldOptScreenModelWithLens optChild address world lens =
        match optChild with
        | None -> setOptChild worldScreenModelAdder worldScreenModelRemover address world None
        | Some child ->
            let optChildModel = getOptChild worldOptScreenModelFinder address world
            match optChildModel with
            | None -> failwith "Cannot change a non-existent screen."
            | Some childModel ->
                let childModel' = set child childModel lens
                setChild worldScreenModelAdder worldScreenModelRemover address world childModel'

    let worldScreenModelLens address =
        { Get = fun world -> Option.get <| worldOptScreenModelFinder address world
          Set = fun screen world -> worldScreenModelAdder address world screen }

    let worldOptScreenModelLens address =
        { Get = fun world -> worldOptScreenModelFinder address world
          Set = fun optScreen world -> match optScreen with None -> worldScreenModelRemover address world | Some screen -> worldScreenModelAdder address world screen }

    let worldScreenModelsLens address =
        { Get = fun world ->
            match address with
            | [] -> world.ScreenModels
            | _ -> failwith <| "Invalid screen model address '" + str address + "'."
          Set = fun screenModels world ->
            match address with
            | [] -> { world with ScreenModels = Map.addMany (Map.toSeq screenModels) world.ScreenModels }
            | _ -> failwith <| "Invalid screen model address '" + str address + "'." }

    let worldScreenLens address =
        { Get = fun world -> getWorldScreenModelWithLens address world screenLens
          Set = fun screen world -> setWorldScreenModelWithLens screen address world screenLens }

    let worldOptScreenLens address =
        { Get = fun world -> getWorldOptScreenModelWithLens address world screenLens
          Set = fun optScreen world -> setWorldOptScreenModelWithLens optScreen address world screenLens }

    let worldIncomingModelLens address = worldScreenModelLens address >>| incomingModelLens
    let worldOutgoingModelLens address = worldScreenModelLens address >>| outgoingModelLens
    
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
          State = IdlingState
          IncomingModel = Transition <| makeDefaultTransition Incoming
          OutgoingModel = Transition <| makeDefaultTransition Outgoing }

    let makeDissolveScreen incomingTime outgoingTime =
        let incomingDissolve = Dissolve <| makeDissolveTransition incomingTime Incoming
        let outgoingDissolve = Dissolve <| makeDissolveTransition outgoingTime Outgoing
        { makeDefaultScreen () with IncomingModel = incomingDissolve; OutgoingModel = outgoingDissolve }