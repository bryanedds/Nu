namespace BlazeVector
open System
open System.Collections
open OpenTK
open Microsoft.Xna
open FarseerPhysics
open FarseerPhysics.Dynamics
open Prime
open Nu

[<AutoOpen>]
module BlazeDispatchersModule =

    /// TODO document.
    type BlazeStageGroupDispatcher () =
        inherit GroupDispatcher ()

        let getCharacterAddress groupAddress =
            groupAddress @ [BlazeConstants.StageCharacterName]

        let getCharacter groupAddress world =
            let characterAddress = getCharacterAddress groupAddress
            get world <| World.worldEntity characterAddress

        let withCharacter fn groupAddress world =
            let characterAddress = getCharacterAddress groupAddress
            World.withEntity fn characterAddress world

        let adjustCamera groupAddress world =
            let character = getCharacter groupAddress world
            let eyeCenter = Vector2 (character.Position.X + character.Size.X * 0.5f, world.Camera.EyeCenter.Y)
            { world with Camera = { world.Camera with EyeCenter = eyeCenter }}

        let adjustCameraHandler _ _ groupAddress message world =
            (message, true, adjustCamera groupAddress world)

        let moveCharacterHandler _ _ groupAddress message world =
            let character = getCharacter groupAddress world
            let linearVelocity = Physics.getLinearVelocity character.PhysicsId world.Integrator
            let linearVelocityDesired = Vector2 (100.0f, linearVelocity.Y)
            let setLinearVelocityMessage = { PhysicsId = character.PhysicsId; LinearVelocity = linearVelocityDesired }
            let world' = { world with PhysicsMessages = SetLinearVelocityMessage setLinearVelocityMessage :: world.PhysicsMessages }
            (message, true, world')

        let jumpCharacterHandler _ _ groupAddress message world =
            let character = getCharacter groupAddress world
            if not <| Physics.isBodyOnGround character.PhysicsId world.Integrator then (message, true, world)
            else
                let applyLinearImpulseMessage = { PhysicsId = character.PhysicsId; LinearImpulse = Vector2 (0.0f, 3000.0f) }
                let world' = { world with PhysicsMessages = ApplyLinearImpulseMessage applyLinearImpulseMessage :: world.PhysicsMessages }
                (message, true, world')

        override dispatcher.Register (group, address, entities, world) =
            let world' =
                world |>
                World.subscribe NuConstants.TickEvent address -<| CustomSub moveCharacterHandler |>
                World.subscribe NuConstants.TickEvent address -<| CustomSub adjustCameraHandler |>
                World.subscribe NuConstants.DownMouseRightEvent address -<| CustomSub jumpCharacterHandler
            let world'' = base.Register (group, address, entities, world')
            adjustCamera address world''

        override dispatcher.Unregister (group, address, world) =
            let world' =
                world |>
                World.unsubscribe NuConstants.TickEvent address |>
                World.unsubscribe NuConstants.TickEvent address |>
                World.unsubscribe NuConstants.DownMouseLeftEvent address
            base.Unregister (group, address, world')

    type BlazeStageScreenDispatcher () =
        inherit ScreenDispatcher ()

        let shiftEntities xShift entities world =
            List.map
                (fun (entity : Entity) ->
                    if Xtension.derivesFrom typeof<Entity2dDispatcher> entity.Xtension world then entity
                    else entity.SetPosition <| entity.Position + Vector2 (xShift, 0.0f))
                entities

        let makeSectionFromFile fileName sectionName xShift world =
            let (sectionGroup, sectionEntities) = World.loadGroupFromFile fileName true world
            let sectionEntities' = shiftEntities xShift sectionEntities world
            (sectionName, sectionGroup, sectionEntities')

        override dispatcher.Register (screen, address, groupDescriptors, world) =
            let stagePlay = World.loadGroupFromFile BlazeConstants.StagePlayFileName true world
            let stagePlayDescriptor = Triple.prepend BlazeConstants.StagePlayName stagePlay
            let sectionDescriptor0 = makeSectionFromFile BlazeConstants.Section0FileName BlazeConstants.Section0Name 0.0f world
            let sectionDescriptor1 = makeSectionFromFile BlazeConstants.Section1FileName BlazeConstants.Section1Name 2048.0f world
            let sectionDescriptor2 = makeSectionFromFile BlazeConstants.Section2FileName BlazeConstants.Section2Name 4096.0f world
            let sectionDescriptor3 = makeSectionFromFile BlazeConstants.Section3FileName BlazeConstants.Section3Name 6144.0f world
            let sectionDescriptors = [sectionDescriptor0; sectionDescriptor1; sectionDescriptor2; sectionDescriptor3]
            let groupDescriptors' = stagePlayDescriptor :: sectionDescriptors @ groupDescriptors
            base.Register (screen, address, groupDescriptors', world)

        override dispatcher.Unregister (screen, address, world) =
            base.Unregister (screen, address, world)

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeGameDispatcher () =
        inherit GameDispatcher ()

        override dispatcher.Register (blazeGame, world) =

            // add the BlazeVector-specific dispatchers to the world
            let dispatchers =
                Map.addMany
                    [|typeof<BlazeStageGroupDispatcher>.Name, BlazeStageGroupDispatcher () :> obj
                      typeof<BlazeStageScreenDispatcher>.Name, BlazeStageScreenDispatcher () :> obj|]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }