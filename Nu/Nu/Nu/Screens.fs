namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu
open Nu.Core
open Nu.DomainModel
module Screens =

    let transitionIdLens =
        { Get = fun (transition : Transition) -> transition.Id
          Set = fun value transition -> { transition with Id = value }}

    let transitionLifetimeLens =
        { Get = fun transition -> transition.Lifetime
          Set = fun value transition -> { transition with Lifetime = value }}

    let transitionTicksLens =
        { Get = fun transition -> transition.Ticks
          Set = fun value transition -> { transition with Ticks = value }}

    let transitionTypeLens =
        { Get = fun transition -> transition.Type
          Set = fun value transition -> { transition with Type = value }}

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

    let screenIncomingLens =
        { Get = fun screenModel -> (get screenModel screenLens).Incoming
          Set = fun value screenModel -> set { get screenModel screenLens with Incoming = value } screenModel screenLens }

    let screenOutgoingLens =
        { Get = fun screenModel -> (get screenModel screenLens).Outgoing
          Set = fun value screenModel -> set { get screenModel screenLens with Outgoing = value } screenModel screenLens }

    let incomingLens =
        { Get = fun screenModel -> (get screenModel screenLens).Incoming
          Set = fun incoming screenModel -> set { (get screenModel screenLens) with Incoming = incoming } screenModel screenLens }

    let outgoingLens =
        { Get = fun screenModel -> (get screenModel screenLens).Outgoing
          Set = fun outgoing screenModel -> set { (get screenModel screenLens) with Outgoing = outgoing } screenModel screenLens }
       
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

    let worldIncomingLens address = worldScreenModelLens address >>| incomingLens
    let worldOutgoingLens address = worldScreenModelLens address >>| outgoingLens
    
    let makeDissolveSprite () =
        { SpriteAssetName = Lun.make "Image8"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

    let makeDefaultTransition transitionType =
        { Id = getNuId ()
          Lifetime = 0
          Ticks = 0
          Type = transitionType
          Sprite = makeDissolveSprite () }

    let makeDefaultScreen () =
        { Id = getNuId ()
          State = IdlingState
          Incoming = makeDefaultTransition Incoming
          Outgoing = makeDefaultTransition Outgoing }

    let makeDissolveScreen incomingTime outgoingTime =
        let incomingDissolve = { makeDefaultTransition Incoming with Lifetime = incomingTime }
        let outgoingDissolve = { makeDefaultTransition Outgoing with Lifetime = outgoingTime }
        { makeDefaultScreen () with Incoming = incomingDissolve; Outgoing = outgoingDissolve }