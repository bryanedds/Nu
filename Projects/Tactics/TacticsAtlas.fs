namespace Tactics
open System
open System.Collections.Generic
open System.Numerics
open Prime
open TiledSharp
open Nu
open Nu.Declarative

type AtlasState =
    | Playing
    | Quitting

[<RequireQualifiedAccess>]
module Atlas =

    type Atlas =
        private
            { AtlasState_ : AtlasState
              SaveSlot_ : SaveSlot
              FieldOpt_ : Field option }

        member this.AtlasState = this.AtlasState_
        member this.SaveSlot = this.SaveSlot_
        member this.FieldOpt = this.FieldOpt_

    let updateAtlasState updater atlas =
        { atlas with AtlasState_ = updater atlas.AtlasState_ }

    let updateFieldOpt updater atlas =
        { atlas with FieldOpt_ = updater atlas.FieldOpt_ }

    let synchronizeFromField field atlas =
        atlas

    let tryLoad saveSlot world =
        None

    let initial saveSlot =
        { AtlasState_ = Playing; SaveSlot_ = saveSlot; FieldOpt_ = None }

type Atlas = Atlas.Atlas