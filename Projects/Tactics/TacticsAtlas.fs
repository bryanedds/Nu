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

    type [<NoComparison>] Atlas =
        private
            { AtlasState_ : AtlasState
              SaveSlot_ : SaveSlot
              FieldOpt_ : Field option }

        (* Local Properties *)
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

    let empty =
        { AtlasState_ = Quitting; SaveSlot_ = Slot1; FieldOpt_ = None }

    let initial saveSlot =
        { AtlasState_ = Playing; SaveSlot_ = saveSlot; FieldOpt_ = None }

    let debug (world : World) =
        let occupants =
            [(v2i 10 10, ChestIndex 0, Chest ())
             (v2i 12 10, ChestIndex 1, Chest ())
             (v2i 10 12, ChestIndex 2, Chest ())
             (v2i 12 12, ChestIndex 3, Chest ())]
        { AtlasState_ = Playing; SaveSlot_ = Slot1; FieldOpt_ = Some (Field.make world.UpdateTime FieldScript.empty occupants (asset "Field" "Field")) }

type Atlas = Atlas.Atlas