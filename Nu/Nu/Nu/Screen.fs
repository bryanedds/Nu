namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu
open Nu.Core
open Nu.DomainModel
module ScreenModule =

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

    let screenIdLens =
        { Get = fun (screen : Screen) -> screen.Id
          Set = fun value screen -> { screen with Id = value }}

    let screenStateLens =
        { Get = fun screen -> screen.State
          Set = fun value screen -> { screen with State = value }}

    let screenIncomingLens =
        { Get = fun screen -> screen.Incoming
          Set = fun value screen -> { screen with Incoming = value }}

    let screenOutgoingLens =
        { Get = fun screen -> screen.Outgoing
          Set = fun value screen -> { screen with Outgoing = value }}

    let incomingLens =
        { Get = fun screen -> screen.Incoming
          Set = fun incoming screen -> { screen with Incoming = incoming }}

    let outgoingLens =
        { Get = fun screen -> screen.Outgoing
          Set = fun outgoing screen -> { screen with Outgoing = outgoing }}
       
    let worldOptScreenFinder (address : Address)  world =
        Map.tryFind address.[0] world.Screens

    let worldScreenAdder (address : Address) world (child : Screen) =
        { world with Screens = Map.add address.[0] child world.Screens }

    let worldScreenRemover (address : Address)  world =
        { world with Screens = Map.remove address.[0] world.Screens }

    let worldScreenLens address =
        { Get = fun world -> Option.get <| worldOptScreenFinder address world
          Set = fun screen world -> worldScreenAdder address world screen }

    let worldOptScreenLens address =
        { Get = fun world -> worldOptScreenFinder address world
          Set = fun optScreen world -> match optScreen with None -> worldScreenRemover address world | Some screen -> worldScreenAdder address world screen }

    let worldScreensLens address =
        { Get = fun world ->
            match address with
            | [] -> world.Screens
            | _ -> failwith <| "Invalid screen address '" + str address + "'."
          Set = fun screens world ->
            match address with
            | [] -> { world with Screens = Map.addMany (Map.toSeq screens) world.Screens }
            | _ -> failwith <| "Invalid screen address '" + str address + "'." }

    let worldIncomingLens address = worldScreenLens address >>| incomingLens
    let worldOutgoingLens address = worldScreenLens address >>| outgoingLens
    
    let makeDissolveSprite () =
        { SpriteAssetName = Lun.make "Image8"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

    let makeDefaultTransition transitionType =
        { Id = getNuId ()
          Lifetime = 0
          Ticks = 0
          Type = transitionType
          OptDissolveSprite = None
          Xtension = { OptName = Some <| Lun.make "TransitionDispatcher"; Fields = Map.empty }}

    let makeDefaultScreen () =
        { Id = getNuId ()
          State = IdlingState
          Incoming = makeDefaultTransition Incoming
          Outgoing = makeDefaultTransition Outgoing
          Xtension = { OptName = Some <| Lun.make "ScreenDispatcher"; Fields = Map.empty }}

    let makeDissolveScreen incomingTime outgoingTime =
        let optDissolveSprite = Some <| makeDissolveSprite ()
        let incomingDissolve = { makeDefaultTransition Incoming with Lifetime = incomingTime; OptDissolveSprite = optDissolveSprite }
        let outgoingDissolve = { makeDefaultTransition Outgoing with Lifetime = outgoingTime; OptDissolveSprite = optDissolveSprite  }
        { makeDefaultScreen () with Incoming = incomingDissolve; Outgoing = outgoingDissolve }