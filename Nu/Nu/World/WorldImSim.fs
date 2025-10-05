// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

/// ImSim API for the world.
[<AutoOpen>]
module WorldImSim =

    /// Instructs ImSim static equality (.=) to act as dynamic equality (@=) for a frame.
    let mutable internal Reinitializing = false

    /// Specifies a static ImSim argument that DOESN'T reinitialize on code reload.
    let
#if !DEBUG
        inline
#endif
        (|=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgType = InitializingArg; ArgLens = lens; ArgValue = value } : 's ArgImSim

    /// Specifies a static ImSim argument that reinitializes on code reload.
    let
#if !DEBUG
        inline
#endif
        (.=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgType = ReinitializingArg; ArgLens = lens; ArgValue = value } : 's ArgImSim

    /// Specifies a dynamic ImSim argument.
    let
#if !DEBUG
        inline
#endif
        (@=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgType = DynamicArg; ArgLens = lens; ArgValue = value } : 's ArgImSim

    type World with

        static member internal tryProcessGame zeroDelta (game : Game) (world : World) =
            let dispatcher = game.GetDispatcher world
            dispatcher.TryProcess (zeroDelta, game, world)

        static member internal tryProcessScreen zeroDelta (screen : Screen) (world : World) =
            let dispatcher = screen.GetDispatcher world
            dispatcher.TryProcess (zeroDelta, screen, world)

        static member internal tryProcessGroup zeroDelta (group : Group) (world : World) =
            let dispatcher = group.GetDispatcher world
            dispatcher.TryProcess (zeroDelta, group, world)

        static member internal tryProcessEntity zeroDelta (entity : Entity) (world : World) =
            let dispatcher = entity.GetDispatcher world
            dispatcher.TryProcess (zeroDelta,  entity, world)

        /// Whether ImSim is reinitializing this frame (such as on a code reload).
        member this.ReinitializingImSim =
            Reinitializing

        /// Whether ImSim is reinitializing this frame (such as on a code reload).
        static member getReinitializingImSim (world : World) =
            world.ReinitializingImSim

        /// ImSim subscribe to the given event address with a user-defined result.
        static member doSubscriptionPlus<'d, 'r> (mapResult : 'd -> 'r) name (eventAddress : 'd Address) (world : World) : 'r FQueue =
            let eventAddress' =
                if not (Array.contains "Event" eventAddress.Names)
                then Address.makeFromArray<'d> (Array.concat [|eventAddress.Names; [|"Event"|]; world.ContextImSim.Names|])
                else eventAddress
            let subscriptionKey = (name, eventAddress :> Address, eventAddress' :> Address)
            match world.SubscriptionsImSim.TryGetValue subscriptionKey with
            | (true, subscriptionImSim) -> World.utilizeSubscriptionImSim subscriptionKey subscriptionImSim world
            | (false, _) ->
                let subId = Gen.id64
                let _ =
                    World.subscribePlus subId (fun event world ->
                        let mapSubscriptionImSim subscriptionImSim =
                            let results = subscriptionImSim.Results :?> 'r FQueue
                            { subscriptionImSim with Results = FQueue.conj (mapResult event.Data) results }
                        World.tryMapSubscriptionImSim mapSubscriptionImSim subscriptionKey world
                        Cascade)
                        eventAddress'
                        Game
                        world
                World.addSubscriptionImSim subscriptionKey { SubscriptionUtilized = true; SubscriptionId = subId; Results = FQueue.empty<'r> } world
            let results = (World.getSubscriptionImSim subscriptionKey world).Results :?> 'r FQueue
            World.mapSubscriptionImSim (fun subscriptionImSim -> { subscriptionImSim with Results = FQueue.empty<'r> }) subscriptionKey world
            results

        /// ImSim subscribe to the given event address.
        static member doSubscription<'d> name (eventAddress : 'd Address) (world : World) : 'd FQueue =
            World.doSubscriptionPlus<'d, 'd> id name eventAddress world

        /// ImSim subscribe to the given event address, resulting in true when any of its events occur.
        static member doSubscriptionAny<'d> name (eventAddress : 'd Address) (world : World) : bool =
            World.doSubscriptionPlus<'d, 'd> id name eventAddress world |> FQueue.notEmpty

        /// ImSim subscribe to the given event address, resulting in true when none of its events occur.
        static member doSubscriptionNone<'d> name (eventAddress : 'd Address) (world : World) : bool =
            World.doSubscriptionPlus<'d, 'd> id name eventAddress world |> FQueue.isEmpty

        /// ImSim subscribe to the given event address, resulting in the number of its events occurred since last time.
        static member doSubscriptionCount<'d> name (eventAddress : 'd Address) (world : World) : int =
            World.doSubscriptionPlus<'d, 'd> id name eventAddress world |> FQueue.length

        /// ImGui subscribe to the given screen's selection events.
        static member doSubscriptionToSelectionEvents name (screen : Screen) (world : World) : SelectionEventData FQueue =
            let selects = World.doSubscriptionPlus (fun () -> (Gen.id64, Select)) name screen.SelectEvent world
            let incomingStarts = World.doSubscriptionPlus (fun () -> (Gen.id64, IncomingStart)) name screen.IncomingStartEvent world
            let incomingFinishes = World.doSubscriptionPlus (fun () -> (Gen.id64, IncomingFinish)) name screen.IncomingFinishEvent world
            let outgoingStarts = World.doSubscriptionPlus (fun () -> (Gen.id64, OutgoingStart)) name screen.OutgoingStartEvent world
            let outgoingFinishes = World.doSubscriptionPlus (fun () -> (Gen.id64, OutgoingFinish)) name screen.OutgoingFinishEvent world
            let deselectings = World.doSubscriptionPlus (fun () -> (Gen.id64, Deselecting)) name screen.DeselectingEvent world
            let results = selects |> Seq.append incomingStarts |> Seq.append incomingFinishes |> Seq.append outgoingStarts |> Seq.append outgoingFinishes |> Seq.append deselectings |> List
            results.Sort (fun (leftId, _) (rightId, _) -> leftId.CompareTo rightId)
            results |> Seq.map snd |> FQueue.ofSeq

        /// ImGui subscribe to the given entity's body events.
        static member doSubscriptionToBodyEvents name (entity : Entity) (world : World) : BodyEventData FQueue =
            let penetrations = World.doSubscriptionPlus (fun data -> (Gen.id64, BodyPenetrationData data)) name entity.BodyPenetrationEvent world
            let separationExplicits = World.doSubscriptionPlus (fun data -> (Gen.id64, BodySeparationExplicitData data)) name entity.BodySeparationExplicitEvent world
            let separationImplicits = World.doSubscriptionPlus (fun data -> (Gen.id64, BodySeparationImplicitData data)) name entity.BodySeparationImplicitEvent world
            let bodyTransforms = World.doSubscriptionPlus (fun data -> (Gen.id64, BodyTransformData data)) name entity.BodyTransformEvent world
            let results = penetrations |> Seq.append separationExplicits |> Seq.append separationImplicits |> Seq.append bodyTransforms |> List
            results.Sort (fun (leftId, _) (rightId, _) -> leftId.CompareTo rightId)
            results |> Seq.map snd |> FQueue.ofSeq

        /// TODO: document this!
        static member initBodyResult mapResult (entity : Entity) world =
            World.monitor (fun event world -> mapResult (FQueue.conj $ BodyPenetrationData event.Data) world; Cascade) entity.BodyPenetrationEvent entity world
            World.monitor (fun event world -> mapResult (FQueue.conj $ BodySeparationExplicitData event.Data) world; Cascade) entity.BodySeparationExplicitEvent entity world
            World.monitor (fun event world -> mapResult (FQueue.conj $ BodySeparationImplicitData event.Data) world; Cascade) entity.BodySeparationImplicitEvent entity world
            World.monitor (fun event world -> mapResult (FQueue.conj $ BodyTransformData event.Data) world; Cascade) entity.BodyTransformEvent entity world

        /// TODO: document this!
        static member initSpineSkeletonAnimationResult mapResult (entity : Entity) world =
            World.monitor (fun event world -> mapResult (FQueue.conj $ event.Data) world; Cascade) entity.SpineSkeletonAnimationTriggerEvent entity world

        /// Clear the current ImSim context.
        static member scopeWorld world =
            World.setContext Address.empty world

        /// Begin the ImSim declaration of a game with the given arguments.
        static member beginGame args (world : World) =
            if world.ContextImSim.Names.Length > 0 then raise (InvalidOperationException "ImSim game declared outside of valid ImSim context (must be called in World context).")
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName world.ContextImSim.Names)
            World.setContext gameAddress world
            let game = Nu.Game gameAddress
            let initializing =
                match world.SimulantsImSim.TryGetValue game.GameAddress with
                | (true, gameImSim) ->
                    World.utilizeSimulantImSim game.GameAddress gameImSim world
                    false
                | (false, _) ->
                    World.addSimulantImSim game.GameAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = () } world
                    true
            for arg in args do
                if (match arg.ArgType with
                    | InitializingArg -> initializing
                    | ReinitializingArg -> initializing || Reinitializing
                    | DynamicArg -> true) then
                    game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore

        /// End the ImSim declaration of a group with the given arguments.
        static member endGame (world : World) =
            match world.ContextImSim with
            | :? (Game Address) -> World.setContext Address.empty world
            | _ -> raise (InvalidOperationException "World.beginGame mismatch.")

        /// ImSim declare a game with the given arguments.
        static member doGame args world =
            World.beginGame args world
            World.endGame world

        /// Make the game the current ImSim context.
        static member scopeGame (args : Game ArgImSim seq) world =
            let game = Game
            World.setContext game.GameAddress world
            for arg in args do
                game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore

        (* NOTE: most of ImSim's screen functions are defined in WorldModule2.fs so they can access the internal screen transition APIs. *)

        /// Make a screen the current ImSim context.
        static member scopeScreen (screen : Screen) (args : Screen ArgImSim seq) world =
            World.setContext screen.ScreenAddress world
            for arg in args do
                if screen.GetExists world then
                    screen.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore

        static member private beginGroupPlus6<'d, 'r when 'd :> GroupDispatcher> (zero : 'r) init name groupFilePathOpt (args : Group ArgImSim seq) (world : World) : 'r =
            Address.assertIdentifierName name
            if world.ContextImSim.Names.Length <> 2 then raise (InvalidOperationException "ImSim group declared outside of valid ImSim context (must be called in a Screen context).")
            let groupAddress = Address.makeFromArray (Array.add name world.ContextImSim.Names)
            World.setContext groupAddress world
            let group = Nu.Group groupAddress
            let groupCreation = not (group.GetExists world)
            let initializing =
                match world.SimulantsImSim.TryGetValue group.GroupAddress with
                | (true, groupImSim) ->
                    World.utilizeSimulantImSim group.GroupAddress groupImSim world
                    false
                | (false, _) ->

                    // init subscriptions _before_ potentially creating group
                    World.addSimulantImSim group.GroupAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = () } world
                    let mapResult (mapper : 'r -> 'r) world =
                        let mapGroupImSim groupImSim = { groupImSim with Result = mapper (groupImSim.Result :?> 'r) }
                        World.tryMapSimulantImSim mapGroupImSim group.GroupAddress world
                    init mapResult group world

                    // create group only when needed
                    if groupCreation then
                        match groupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath (Some name) group.Screen world |> ignore<Group>
                        | None -> World.createGroup5 true typeof<'d>.Name (Some name) group.Screen world |> ignore<Group>

                    // protect group
                    World.setGroupProtected true group world |> ignore<bool>

                    // fin
                    true

            for arg in args do
                if (match arg.ArgType with
                    | InitializingArg -> initializing
                    | ReinitializingArg -> initializing || Reinitializing
                    | DynamicArg -> true) && group.GetExists world then
                    group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore
            if groupCreation && group.GetExists world && WorldModule.UpdatingSimulants && World.getGroupSelected group world then
                WorldModule.tryProcessGroup true group world
            let result = match (World.getSimulantImSim group.GroupAddress world).Result with :? 'r as r -> r | _ -> zero
            World.mapSimulantImSim (fun simulantImSim -> { simulantImSim with Result = zero }) group.GroupAddress world
            result

        static member inline private beginGroup4<'d> name groupFilePathOpt args world =
            World.beginGroupPlus6 () (fun _ _ _ -> ()) name groupFilePathOpt args world

        /// Begin the ImSim declaration of a group read from the given file path with the given arguments.
        /// Note that changing the file path over time has no effect as only the first moment is used.
        static member beginGroupFromFile (name : string) (groupFilePath : string) args (world : World) =
            Address.assertIdentifierName name
            if world.ContextImSim.Names.Length <> 2 then raise (InvalidOperationException "ImSim group declared outside of valid ImSim context (must be called in a Screen context).")
            let groupAddress = Address.makeFromArray (Array.add name world.ContextImSim.Names)
            World.setContext groupAddress world
            let group = Nu.Group groupAddress
            // HACK: when group appears to exist as a placeholder created by Gaia, we destroy it so it can be made in a user-defined way.
            if  group.Name = "Scene" &&
                group.GetExists world &&
                Seq.isEmpty (World.getSovereignEntities group world) &&
                getTypeName (group.GetDispatcher world) = nameof GroupDispatcher then
                World.destroyGroupImmediate group world
            let groupCreation = not (group.GetExists world)
            let initializing =
                match world.SimulantsImSim.TryGetValue group.GroupAddress with
                | (true, groupImSim) ->
                    World.utilizeSimulantImSim group.GroupAddress groupImSim world
                    false
                | (false, _) ->
                    if groupCreation then
                        let groupDescriptorStr = File.ReadAllText groupFilePath
                        let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                        World.readGroup groupDescriptor (Some name) group.Screen world |> ignore<Group>
                        World.setGroupProtected true group world |> ignore<bool>
                    World.addSimulantImSim group.GroupAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = () } world
                    true
            for arg in args do
                if (match arg.ArgType with
                    | InitializingArg -> initializing
                    | ReinitializingArg -> initializing || Reinitializing
                    | DynamicArg -> true) && group.GetExists world then
                    group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore
            if groupCreation && group.GetExists world && WorldModule.UpdatingSimulants && World.getGroupSelected group world then
                WorldModule.tryProcessGroup true group world

        /// Begin the ImSim declaration of a group with the given arguments.
        static member beginGroupPlus<'d, 'r when 'd :> GroupDispatcher> zero init name args world =
            World.beginGroupPlus6<'d, 'r> zero init name None args world

        /// Begin the ImSim declaration of a group with the given arguments.
        static member beginGroup<'d when 'd :> GroupDispatcher> name args world =
            World.beginGroup4<'d> name None args world

        /// End the ImSim declaration of a group.
        static member endGroup (world : World) =
            match world.ContextImSim with
            | :? (Group Address) as groupAddress ->
                let currentAddress = Address.take<Group, Screen> 2 groupAddress
                World.setContext currentAddress world
            | _ -> raise (InvalidOperationException "World.beginGroup mismatch.")

        /// ImSim declare a group with the given arguments.
        static member doGroupPlus<'d, 'r when 'd :> GroupDispatcher> zero init name groupFilePathOpt args world =
            let result = World.beginGroupPlus6<'d, 'r> zero init name groupFilePathOpt args world
            World.endGroup world
            result

        /// ImSim declare a group read from the given file path with the given arguments.
        static member doGroupFromFile name groupFilePath args world =
            World.beginGroupFromFile name groupFilePath args world
            World.endGroup world

        /// ImSim declare a group with the given arguments.
        static member doGroup<'d when 'd :> GroupDispatcher> name args world =
            World.beginGroup4<'d> name None args world
            World.endGroup world

        /// Make a group the current ImSim context.
        static member scopeGroup (group : Group) (args : Group ArgImSim seq) world =
            World.setContext group.GroupAddress world
            for arg in args do
                if group.GetExists world then
                    group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore

        /// Begin the ImSim declaration of a entity read from the given file path with the given arguments.
        /// Note that changing the file path over time has no effect as only the first moment is used.
        static member beginEntityFromFile name entityFilePath args (world : World) =
            
            // decide on entity creation
            Address.assertIdentifierName name
            if world.ContextImSim.Names.Length < 3 then raise (InvalidOperationException "ImSim entity declared outside of valid ImSim context (must be called in a Group or Entity context).")
            let entityAddress = Address.makeFromArray (Array.add name world.ContextImSim.Names)
            World.setContext entityAddress world
            let entity = Nu.Entity entityAddress
            let entityCreation = not (entity.GetExists world)

            // create entity when appropriate
            let initializing =
                match world.SimulantsImSim.TryGetValue entity.EntityAddress with
                | (true, entityImSim) ->
                    World.utilizeSimulantImSim entity.EntityAddress entityImSim world
                    false
                | (false, _) ->
                    if entityCreation then
                        let entityDescriptorStr = File.ReadAllText entityFilePath
                        let entityDescriptor = scvalue<EntityDescriptor> entityDescriptorStr
                        World.readEntity false true entityDescriptor (Some name) entity.Parent world |> ignore<Entity>
                        World.setEntityProtected true entity world |> ignore<bool>
                    World.addSimulantImSim entity.EntityAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = () } world
                    true

            // entity-specific initialization
            let mutable mountOptOpt = ValueNone
            for arg in args do
                if arg.ArgLens.Name = Constants.Engine.MountOptPropertyName then
                    mountOptOpt <- ValueSome (arg.ArgValue :?> Entity Address option)
                if (match arg.ArgType with
                    | InitializingArg -> initializing
                    | ReinitializingArg -> initializing || Reinitializing
                    | DynamicArg -> true) && entity.GetExists world then
                    entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore
            
            // update mount opt when appropriate
            if mountOptOpt.IsNone && (initializing || Reinitializing) && entity.GetExists world && entity.Surnames.Length > 1 then
                entity.SetMountOpt (Some Address.parent) world
            
            // process entity when appropriate
            if entityCreation && entity.GetExists world && WorldModule.UpdatingSimulants && World.getEntitySelected entity world then
                WorldModule.tryProcessEntity true entity world

        /// Begin the ImSim declaration of an entity with the given arguments.
        static member beginEntityPlus<'d, 'r when 'd :> EntityDispatcher> (zero : 'r) init name (args : Entity ArgImSim seq) (world : World) : 'r =
            
            // decide on entity creation
            Address.assertIdentifierName name
            if world.ContextImSim.Names.Length < 3 then raise (InvalidOperationException "ImSim entity declared outside of valid ImSim context (must be called in either Group or Entity context).")
            let entityAddress = Address.makeFromArray (Array.add name world.ContextImSim.Names)
            World.setContext entityAddress world
            let entity = Nu.Entity entityAddress
            let entityCreation = not (entity.GetExists world)

            // attempt to find mountOpt
            // NOTE: this is an additional iteration of args including the one below, so it's a potential performance drain.
            let mutable mountOptOpt = ValueNone
            for arg in args do
                if arg.ArgLens.Name = Constants.Engine.MountOptPropertyName then
                    mountOptOpt <- ValueSome (arg.ArgValue :?> Entity Address option)

            // create entity when appropriate
            let initializing =
                match world.SimulantsImSim.TryGetValue entity.EntityAddress with
                | (true, entityImSim) ->
                    World.utilizeSimulantImSim entity.EntityAddress entityImSim world
                    false
                | (false, _) ->

                    // init subscriptions _before_ potentially creating entity
                    World.addSimulantImSim entity.EntityAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = zero } world
                    let mapResult (mapper : 'r -> 'r) world =
                        let mapEntityImSim entityImSim = { entityImSim with Result = mapper (entityImSim.Result :?> 'r) }
                        World.tryMapSimulantImSim mapEntityImSim entity.EntityAddress world
                    init mapResult entity world

                    // create entity only when needed
                    if entityCreation then
                        let mountOpt = match mountOptOpt with ValueSome mountOpt -> mountOpt | ValueNone -> Some Address.parent
                        World.createEntity7 true typeof<'d>.Name mountOpt OverlayNameDescriptor.DefaultOverlay (Some entity.Surnames) entity.Group world |> ignore<Entity>

                    // protect entity
                    World.setEntityProtected true entity world |> ignore<bool>
                    true

            // entity-specific initialization
            for arg in args do
                if (match arg.ArgType with
                    | InitializingArg -> initializing
                    | ReinitializingArg -> initializing || Reinitializing
                    | DynamicArg -> true) && entity.GetExists world then
                    entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore

            // update mount opt when appropriate
            if mountOptOpt.IsNone && (initializing || Reinitializing) && entity.GetExists world && entity.Surnames.Length > 1 then
                entity.SetMountOpt (Some Address.parent) world

            // process entity when appropriate
            if entityCreation && entity.GetExists world && WorldModule.UpdatingSimulants && World.getEntitySelected entity world then
                WorldModule.tryProcessEntity true entity world

            // update result
            let result = match (World.getSimulantImSim entity.EntityAddress world).Result with :? 'r as r -> r | _ -> zero
            World.mapSimulantImSim (fun simulantImSim -> { simulantImSim with Result = zero }) entity.EntityAddress world
            result

        /// Begin the ImSim declaration of an entity with the given arguments.
        static member beginEntity<'d when 'd :> EntityDispatcher> name args world =
            World.beginEntityPlus<'d, unit> () (fun _ _ _ -> ()) name args world

        /// End the ImSim declaration of an entity.
        static member endEntity (world : World) =
            match world.ContextImSim with
            | :? (Entity Address) as entityAddress when entityAddress.Length >= 4 ->
                let currentNames = Array.take (dec entityAddress.Length) entityAddress.Names
                let currentAddress =
                    if currentNames.Length = 3
                    then Address.makeFromArray<Group> currentNames :> Address
                    else Address.makeFromArray<Entity> currentNames
                World.setContext currentAddress world
            | _ -> raise (InvalidOperationException "World.beginEntity mismatch.")

        /// ImSim declare an entity read from the given file path with the given arguments.
        /// Note that changing the file path over time has no effect as only the first moment is used.
        static member doEntityFromFile name entityFilePath args world =
            World.beginEntityFromFile name entityFilePath args world
            World.endEntity world

        /// ImSim declare an entity with the given arguments.
        static member doEntityPlus<'d, 'r when 'd :> EntityDispatcher> zero init name args world =
            let result = World.beginEntityPlus<'d, 'r> zero init name args world
            World.endEntity world
            result

        /// ImSim declare an entity with the given arguments.
        static member doEntity<'d when 'd :> EntityDispatcher> name args world =
            World.beginEntity<'d> name args world
            World.endEntity world

        /// Make an entity the current ImSim context.
        static member scopeEntity (entity : Entity) (args : Entity ArgImSim seq) world =
            World.setContext entity.EntityAddress world
            for arg in args do
                if entity.GetExists world then
                    entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore

        /// Begin the ImSim declaration of associated gui entities with the given arguments.
        static member beginAssociation name args world = World.beginEntity<GuiDispatcher> name args world

        /// End the ImSim declaration of associated gui entities.
        static member endAssociation world = World.endEntity world

        /// ImSim declare an empty association of gui entities with the given arguments.
        static member doAssociation name args world = World.doEntity<GuiDispatcher> name args world

        /// <summary>
        /// ImSim declare a basic static sprite emitter with the given arguments.
        /// See <see cref="BasicStaticSpriteEmitterDispatcher" />.
        /// </summary>
        static member doBasicStaticSpriteEmitter name args world = World.doEntity<BasicStaticSpriteEmitterDispatcher> name args world

        /// <summary>
        /// ImSim declare a 2d effect with the given arguments.
        /// See <see cref="Effect2dDispatcher" />.
        /// </summary>
        static member doEffect2d name args world = World.doEntity<Effect2dDispatcher> name args world

        /// <summary>
        /// ImSim declare a static sprite with the given arguments.
        /// See <see cref="StaticSpriteDispatcher" />.
        /// </summary>
        static member doStaticSprite name args world = World.doEntity<StaticSpriteDispatcher> name args world

        /// <summary>
        /// ImSim declare an animated sprite with the given arguments.
        /// See <see cref="AnimatedSpriteDispatcher" />.
        /// </summary>
        static member doAnimatedSprite name args world = World.doEntity<AnimatedSpriteDispatcher> name args world

        /// <summary>
        /// ImSim declare a text entity with the given arguments.
        /// See <see cref="TextDispatcher" />.
        /// </summary>
        static member doText name args world = World.doEntity<TextDispatcher> name args world

        /// <summary>
        /// ImSim declare a label with the given arguments.
        /// See <see cref="LabelDispatcher" />.
        /// </summary>
        static member doLabel name args world = World.doEntity<LabelDispatcher> name args world

        /// <summary>
        /// ImSim declare a button with the given arguments.
        /// See <see cref="ButtonDispatcher" />.
        /// </summary>
        static member doButton name args world =
            let init updateResult (entity : Entity) world = World.monitor (fun _ world -> updateResult tautology world; Cascade) entity.ClickEvent entity world
            World.doEntityPlus<ButtonDispatcher, _> false init name args world

        /// <summary>
        /// ImSim declare a toggle button with the given arguments.
        /// See <see cref="ToggleButtonDispatcher" />.
        /// </summary>
        static member doToggleButton name args world =
            let init updateResult (entity : Entity) world = World.monitor (fun _ world -> updateResult tautology world; Cascade) entity.ToggleEvent entity world
            let toggleChanged = World.doEntityPlus<ToggleButtonDispatcher, _> false init name args world
            (world.DeclaredEntity.GetToggled world, toggleChanged)

        /// <summary>
        /// ImSim declare a radio button with the given arguments.
        /// See <see cref="RadioButtonDispatcher" />.
        /// </summary>
        static member doRadioButton name args world =
            let init updateResult (entity : Entity) world = World.monitor (fun _ world -> updateResult tautology world; Cascade) entity.DialEvent entity world
            let dialChanged = World.doEntityPlus<RadioButtonDispatcher, _> false init name args world
            (world.DeclaredEntity.GetDialed world, dialChanged)

        /// <summary>
        /// ImSim declare a fill bar with the given arguments.
        /// See <see cref="FillBarDispatcher" />.
        /// </summary>
        static member doFillBar name args world = World.doEntity<FillBarDispatcher> name args world

        /// <summary>
        /// ImSim declare a feeler with the given arguments.
        /// See <see cref="FeelerDispatcher" />.
        /// </summary>
        static member doFeeler name args world =
            let init updateResult (entity : Entity) world = World.monitor (fun _ world -> updateResult tautology world; Cascade) entity.TouchEvent entity world
            let touchChanged = World.doEntityPlus<FeelerDispatcher, _> false init name args world
            (world.DeclaredEntity.GetTouched world, touchChanged)

        /// <summary>
        /// ImSim declare a text box entity with the given arguments.
        /// See <see cref="TextBoxDispatcher" />.
        /// </summary>
        static member doTextBox name args world =
            let init updateResult (entity : Entity) world = World.monitor (fun _ world -> updateResult tautology world; Cascade) entity.TextEditEvent entity world
            let textChanged = World.doEntityPlus<TextBoxDispatcher, _> false init name args world
            (world.DeclaredEntity.GetText world, textChanged)

        /// <summary>
        /// ImSim declare an fps entity with the given arguments.
        /// See <see cref="FpsDispatcher" />.
        /// </summary>
        static member doFps name args world = World.doEntity<FpsDispatcher> name args world

        /// <summary>
        /// ImSim declare the beginning of a panel with the given arguments.
        /// See <see cref="PanelDispatcher" />.
        /// </summary>
        static member beginPanel name args world = World.beginEntity<PanelDispatcher> name args world

        /// <summary>
        /// ImSim declare the end of a panel.
        /// See <see cref="PanelDispatcher" />.
        /// </summary>
        static member endPanel world = World.endEntity world

        /// <summary>
        /// ImSim declare a panel with the given arguments.
        /// See <see cref="PanelDispatcher" />.
        /// </summary>
        static member doPanel name args world = World.doEntity<PanelDispatcher> name args world

        /// <summary>
        /// ImSim declare a cursor with the given arguments.
        /// See <see cref="CursorDispatcher" />.
        /// </summary>
        static member doCursor name args world = World.doEntity<CursorDispatcher> name args world

        /// <summary>
        /// ImSim declare a 2d block with the given arguments.
        /// See <see cref="Block2dDispatcher" />.
        /// </summary>
        static member doBlock2d name args world =
            let results = World.doEntityPlus<Block2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 2d box with the given arguments.
        /// See <see cref="Box2dDispatcher" />.
        /// </summary>
        static member doBox2d name args world =
            let results = World.doEntityPlus<Box2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 2d sphere with the given arguments.
        /// See <see cref="Sphere2dDispatcher" />.
        /// </summary>
        static member doSphere2d name args world =
            let results = World.doEntityPlus<Sphere2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 2d ball with the given arguments.
        /// See <see cref="Ball2dDispatcher" />.
        /// </summary>
        static member doBall2d name args world =
            let results = World.doEntityPlus<Ball2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 2d character with the given arguments.
        /// See <see cref="Character2dDispatcher" />.
        /// </summary>
        static member doCharacter2d name args world =
            let results = World.doEntityPlus<Character2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 2d body joint with the given arguments.
        /// See <see cref="BodyJoint2dDispatcher" />.
        /// </summary>
        static member doBodyJoint2d name args world =
            let results = World.doEntityPlus<BodyJoint2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyJointId world, results)

        /// <summary>
        /// ImSim declare a tile map with the given arguments.
        /// See <see cref="TileMapDispatcher" />.
        /// </summary>
        static member doTileMap name args world =
            let results = World.doEntityPlus<TileMapDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a user-defined tile map with the given arguments.
        /// See <see cref="TmxMapDispatcher" />.
        /// </summary>
        static member doTmxMap name args world =
            let results = World.doEntityPlus<TmxMapDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a tile map with the given arguments.
        /// See <see cref="SpineSkeletonDispatcher" />.
        /// </summary>
        static member doSpineSkeleton name args world =
            World.doEntityPlus<SpineSkeletonDispatcher, _> FQueue.empty World.initSpineSkeletonAnimationResult name args world

        /// <summary>
        /// ImSim declare a 3d light probe with the given arguments.
        /// See <see cref="LightProbe3dDispatcher" />.
        /// </summary>
        static member doLightProbe3d name args world = World.doEntity<LightProbe3dDispatcher> name args world

        /// <summary>
        /// ImSim declare a 3d light with the given arguments.
        /// See <see cref="Light3dDispatcher" />.
        /// </summary>
        static member doLight3d name args world = World.doEntity<Light3dDispatcher> name args world

        /// <summary>
        /// ImSim declare a sky box with the given arguments.
        /// See <see cref="SkyBoxDispatcher" />.
        /// </summary>
        static member doSkyBox name args world = World.doEntity<SkyBoxDispatcher> name args world

        /// <summary>
        /// ImSim declare a basic static billboard emitter with the given arguments.
        /// See <see cref="BasicStaticBillboardEmitterDispatcher" />.
        /// </summary>
        static member doBasicStaticBillboardEmitter name args world = World.doEntity<BasicStaticBillboardEmitterDispatcher> name args world

        /// <summary>
        /// ImSim declare a 3d effect with the given arguments.
        /// See <see cref="Effect3dDispatcher" />.
        /// </summary>
        static member doEffect3d name args world = World.doEntity<Effect3dDispatcher> name args world

        /// <summary>
        /// ImSim declare a 3d block with the given arguments.
        /// See <see cref="Block3dDispatcher" />.
        /// </summary>
        static member doBlock3d name args world =
            let results = World.doEntityPlus<Block3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 3d box with the given arguments.
        /// See <see cref="Box3dDispatcher" />.
        /// </summary>
        static member doBox3d name args world =
            let results = World.doEntityPlus<Box3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 3d sphere with the given arguments.
        /// See <see cref="Sphere3dDispatcher" />.
        /// </summary>
        static member doSphere3d name args world =
            let results = World.doEntityPlus<Sphere3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 3d ball with the given arguments.
        /// See <see cref="Ball3dDispatcher" />.
        /// </summary>
        static member doBall3d name args world =
            let results = World.doEntityPlus<Ball3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a static billboard with the given arguments.
        /// See <see cref="StaticBillboardDispatcher" />.
        /// </summary>
        static member doStaticBillboard name args world = World.doEntity<StaticBillboardDispatcher> name args world

        /// <summary>
        /// ImSim declare an animated billboard with the given arguments.
        /// See <see cref="AnimatedBillboardDispatcher" />.
        /// </summary>
        static member doAnimatedBillboard name args world = World.doEntity<AnimatedBillboardDispatcher> name args world

        /// <summary>
        /// ImSim declare a static model with the given arguments.
        /// See <see cref="StaticModelDispatcher" />.
        /// </summary>
        static member doStaticModel name args world = World.doEntity<StaticModelDispatcher> name args world

        /// <summary>
        /// ImSim declare a animated model with the given arguments.
        /// See <see cref="AnimatedModelDispatcher" />.
        /// </summary>
        static member doAnimatedModel name args world = World.doEntity<AnimatedModelDispatcher> name args world

        /// <summary>
        /// ImSim declare a sensor model with the given arguments.
        /// See <see cref="SensorModelDispatcher" />.
        /// </summary>
        static member doSensorModel name args world =
            let results = World.doEntityPlus<SensorModelDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a rigid model with the given arguments.
        /// See <see cref="RigidModelDispatcher" />.
        /// </summary>
        static member doRigidModel name args world =
            let results = World.doEntityPlus<RigidModelDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a static model surface with the given arguments.
        /// See <see cref="StaticModelSurfaceDispatcher" />.
        /// </summary>
        static member doStaticModelSurface name args world = World.doEntity<StaticModelSurfaceDispatcher> name args world

        /// <summary>
        /// ImSim declare a sensor model surface with the given arguments.
        /// See <see cref="SensorModelSurfaceDispatcher" />.
        /// </summary>
        static member doSensorModelSurface name args world =
            let results = World.doEntityPlus<SensorModelSurfaceDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a rigid model surface with the given arguments.
        /// See <see cref="RigidModelSurfaceDispatcher" />.
        /// </summary>
        static member doRigidModelSurface name args world =
            let results = World.doEntityPlus<RigidModelSurfaceDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 3d character with the given arguments.
        /// See <see cref="Character3dDispatcher" />.
        /// </summary>
        static member doCharacter3d name args world =
            let results = World.doEntityPlus<Character3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 3d body joint with the given arguments.
        /// See <see cref="BodyJoint3dDispatcher" />.
        /// </summary>
        static member doBodyJoint3d name args world =
            let results = World.doEntityPlus<BodyJoint3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyJointId world, results)

        /// <summary>
        /// ImSim declare a terrain with the given arguments.
        /// See <see cref="TerrainDispatcher" />.
        /// </summary>
        static member doTerrain name args world =
            let results = World.doEntityPlus<TerrainDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results)

        /// <summary>
        /// ImSim declare a 3d nav config with the given arguments.
        /// See <see cref="Nav3dConfigDispatcher" />.
        /// </summary>
        static member doNav3dConfig name args world = World.doEntity<Nav3dConfigDispatcher> name args world

        /// <summary>
        /// ImSim declare a 3d light config with the given arguments.
        /// See <see cref="Lighting3dConfigDispatcher" />.
        /// </summary>
        static member doLighting3dConfig name args world = World.doEntity<Lighting3dConfigDispatcher> name args world

        /// <summary>
        /// ImSim declare a static model hierarchy with the given arguments.
        /// See <see cref="StaticModelHierarchyDispatcher" />.
        /// </summary>
        static member doStaticModelHierarchy name args world = World.doEntity<StaticModelHierarchyDispatcher> name args world

        /// <summary>
        /// ImSim declare a rigid model hierarchy with the given arguments.
        /// See <see cref="RigidModelHierarchyDispatcher" />.
        /// </summary>
        static member doRigidModelHierarchy name args world = World.doEntity<RigidModelHierarchyDispatcher> name args world