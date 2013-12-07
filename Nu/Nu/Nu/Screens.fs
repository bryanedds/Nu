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
      OutgoingModel : TransitionModel }

type [<StructuralEquality; NoComparison; CLIMutable>] OmniBattleScreen =
    { Screen : Screen
      Battle : OmniBattle }

type [<StructuralEquality; NoComparison>] ScreenModel =
    | Screen of Screen
    | OmniBattleScreen of OmniBattleScreen

module Screens =
    
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