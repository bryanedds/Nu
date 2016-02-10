// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldClipboard =

    // Mutable clipboard that allows its state to persist beyond undo / redo.
    let private RefClipboard : EntityState option ref = ref None

    type World with
        
        /// Copy an entity to the clipboard.
        /// TODO: document mutation quite explicitly.
        static member copyToClipboard entity world =
            let entityState = World.getEntityState entity world
            RefClipboard := Some entityState

        /// Cut an entity to the clipboard.
        /// TODO: document mutation quite explicitly.
        static member cutToClipboard entity world =
            World.copyToClipboard entity world
            World.destroyEntityImmediate entity world

        /// Paste an entity from the clipboard.
        /// TODO: document mutation quite explicitly.
        static member pasteFromClipboard atMouse rightClickPosition positionSnap rotationSnap group world =
            match !RefClipboard with
            | Some entityState ->
                let id = Core.makeId ()
                let name = Name.make ^ acstring id
                let entityState = { entityState with Id = id; Name = name }
                let camera = World.getCamera world
                let entityPosition =
                    if atMouse
                    then Camera.mouseToWorld entityState.ViewType rightClickPosition camera
                    else Camera.mouseToWorld entityState.ViewType (camera.EyeSize * 0.5f) camera
                let entityTransform =
                    { Transform.Position = entityPosition
                      Size = entityState.Size
                      Rotation = entityState.Rotation
                      Depth = entityState.Depth }
                let entityTransform = Math.snapTransform positionSnap rotationSnap entityTransform
                let entityState =
                    { entityState with
                        Position = entityTransform.Position
                        Size = entityTransform.Size
                        Rotation = entityTransform.Rotation
                        Depth = entityTransform.Depth }
                let entity = gtoe group name
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)