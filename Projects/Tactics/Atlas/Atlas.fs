namespace Tactics
open System
open Prime
open Nu

type AtlasState =
    | Playing
    | Quitting
    | Quit

[<RequireQualifiedAccess>]
module Atlas =

    type [<ReferenceEquality; SymbolicExpansion>] Atlas =
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
        { AtlasState_ = Quit; SaveSlot_ = Slot1; FieldOpt_ = None }

    let initial saveSlot =
        { AtlasState_ = Playing; SaveSlot_ = saveSlot; FieldOpt_ = None }

    let debug (world : World) =
        let field = Field.debug world
        { AtlasState_ = Playing; SaveSlot_ = Slot1; FieldOpt_ = Some field }

type Atlas = Atlas.Atlas