namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldClipboard =

    type World with
        
        /// Copy an entity to the clipboard.
        /// TODO: document mutation quite explicitly.
        static member copyToClipboard entity world =
            let entityState = World.getEntityState entity world
            world.State.RefClipboard := Some entityState

        /// Cut an entity to the clipboard.
        /// TODO: document mutation quite explicitly.
        static member cutToClipboard entity world =
            World.copyToClipboard entity world
            World.destroyEntityImmediate entity world

        /// Paste an entity from the clipboard.
        /// TODO: document mutation quite explicitly.
        static member pasteFromClipboard atMouse rightClickPosition positionSnap rotationSnap group world =
            match !world.State.RefClipboard with
            | Some entityState ->
                let id = Core.makeId ()
                let name = acstring id
                let entityState = { entityState with Id = id; Name = name }
                let camera = World.getCamera world
                let entityPosition =
                    if atMouse
                    then Camera.mouseToWorld entityState.ViewType rightClickPosition camera
                    else Camera.mouseToWorld entityState.ViewType (camera.EyeSize * 0.5f) camera
                let entityTransform =
                    { Transform.Position = entityPosition
                      Depth = entityState.Depth
                      Size = entityState.Size
                      Rotation = entityState.Rotation }
                let entityTransform = Math.snapTransform positionSnap rotationSnap entityTransform
                let entityState =
                    { entityState with
                        Position = entityTransform.Position
                        Depth = entityTransform.Depth
                        Size = entityTransform.Size
                        Rotation = entityTransform.Rotation }
                let entity = gtoe group name
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)