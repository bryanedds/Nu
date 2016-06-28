// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldClipboardModule =

    // Mutable clipboard that allows its state to persist beyond undo / redo.
    let private RefClipboard = ref<EntityState option> None

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
                let id = makeGuid ()
                let name = Name.make ^ scstring id
                let entityState = { entityState with Name = name; Id = id }
                let camera = World.getCamera world
                let position =
                    if atMouse
                    then Camera.mouseToWorld entityState.ViewType rightClickPosition camera
                    else Camera.mouseToWorld entityState.ViewType (camera.EyeSize * 0.5f) camera
                let transform = { EntityState.getTransform entityState with Position = position }
                let transform = Math.snapTransform positionSnap rotationSnap transform
                let entityState = EntityState.setTransform transform entityState
                let entity = gtoe group name
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)