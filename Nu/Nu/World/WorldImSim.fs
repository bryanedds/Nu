// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WorldImSim =

    /// Instructs ImSim static equality (.=) to act as dynamic equality (@=) for a frame.
    let mutable internal Reinitializing = false

    /// Specifies a static ImSim argument.
    let
#if !DEBUG
        inline
#endif
        (.=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgStatic = true; ArgLens = lens; ArgValue = value } : 's ArgImSim

    /// Specifies a dynamic ImSim argument.
    let
#if !DEBUG
        inline
#endif
        (@=) (lens : Lens<'a, 's>) (value : 'a) =
        { ArgStatic = false; ArgLens = lens; ArgValue = value } : 's ArgImSim

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

        /// ImSim subscribe to the given event address with a user-defined result mapper.
        static member doSubscriptionPlus<'d, 'r> (mapResult : 'd -> 'r) name (eventAddress : 'd Address) (world : World) : 'r FQueue * World =
            let eventAddress' =
                if not (Array.contains "Event" eventAddress.Names)
                then Address.makeFromArray<'d> (Array.concat [|eventAddress.Names; [|"Event"|]; world.ContextImSim.Names|])
                else eventAddress
            let subscriptionKey = (name, eventAddress :> Address, eventAddress' :> Address)
            let world =
                match world.SubscriptionsImSim.TryGetValue subscriptionKey with
                | (true, subscriptionImSim) -> World.utilizeSubscriptionImSim subscriptionKey subscriptionImSim world
                | (false, _) ->
                    let subId = Gen.id64
                    let (_, world) =
                        World.subscribePlus subId (fun event world ->
                            let mapSubscriptionImSim subscriptionImSim =
                                let results = subscriptionImSim.Results :?> 'r FQueue
                                { subscriptionImSim with Results = FQueue.conj (mapResult event.Data) results }
                            let world = World.tryMapSubscriptionImSim mapSubscriptionImSim subscriptionKey world
                            (Cascade, world))
                            eventAddress'
                            Game
                            world
                    World.addSubscriptionImSim subscriptionKey { SubscriptionUtilized = true; SubscriptionId = subId; Results = FQueue.empty<'r> } world
            let results = (World.getSubscriptionImSim subscriptionKey world).Results :?> 'r FQueue
            let world = World.mapSubscriptionImSim (fun subscriptionImSim -> { subscriptionImSim with Results = FQueue.empty<'r> }) subscriptionKey world
            (results, world)

        /// ImSim subscribe to the given event address.
        static member doSubscription<'d> name (eventAddress : 'd Address) (world : World) : 'd FQueue * World =
            World.doSubscriptionPlus<'d, 'd> id name eventAddress world

        /// ImGui subscribe to the given screen's selection events.
        static member doSubscriptionToSelectionEvents name (screen : Screen) (world : World) : SelectionEventData FQueue * World =
            let (selects, world) = World.doSubscriptionPlus (fun () -> (Gen.id64, Select)) name screen.SelectEvent world
            let (incomingStarts, world) = World.doSubscriptionPlus (fun () -> (Gen.id64, IncomingStart)) name screen.IncomingStartEvent world
            let (incomingFinishes, world) = World.doSubscriptionPlus (fun () -> (Gen.id64, IncomingFinish)) name screen.IncomingFinishEvent world
            let (outgoingStarts, world) = World.doSubscriptionPlus (fun () -> (Gen.id64, OutgoingStart)) name screen.OutgoingStartEvent world
            let (outgoingFinishes, world) = World.doSubscriptionPlus (fun () -> (Gen.id64, OutgoingFinish)) name screen.OutgoingFinishEvent world
            let (deselectings, world) = World.doSubscriptionPlus (fun () -> (Gen.id64, Deselecting)) name screen.DeselectingEvent world
            let results = selects |> Seq.append incomingStarts |> Seq.append incomingFinishes |> Seq.append outgoingStarts |> Seq.append outgoingFinishes |> Seq.append deselectings |> List
            results.Sort (fun (leftId, _) (rightId, _) -> leftId.CompareTo rightId)
            let results = results |> Seq.map snd |> FQueue.ofSeq
            (results, world)

        /// ImGui subscribe to the given entity's body events.
        static member doSubscriptionToBodyEvents name (entity : Entity) (world : World) : BodyEventData FQueue * World =
            let (penetrations, world) = World.doSubscriptionPlus (fun data -> (Gen.id64, BodyPenetrationData data)) name entity.BodyPenetrationEvent world
            let (separationExplicits, world) = World.doSubscriptionPlus (fun data -> (Gen.id64, BodySeparationExplicitData data)) name entity.BodySeparationExplicitEvent world
            let (separationImplicits, world) = World.doSubscriptionPlus (fun data -> (Gen.id64, BodySeparationImplicitData data)) name entity.BodySeparationImplicitEvent world
            let (bodyTransforms, world) = World.doSubscriptionPlus (fun data -> (Gen.id64, BodyTransformData data)) name entity.BodyTransformEvent world
            let results = penetrations |> Seq.append separationExplicits |> Seq.append separationImplicits |> Seq.append bodyTransforms |> List
            results.Sort (fun (leftId, _) (rightId, _) -> leftId.CompareTo rightId)
            let results = results |> Seq.map snd |> FQueue.ofSeq
            (results, world)

        /// TODO: document this!
        static member initBodyResult mapResult (entity : Entity) world =
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodyPenetrationData event.Data) world)) entity.BodyPenetrationEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodySeparationExplicitData event.Data) world)) entity.BodySeparationExplicitEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodySeparationImplicitData event.Data) world)) entity.BodySeparationImplicitEvent entity world
            let world = World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ BodyTransformData event.Data) world)) entity.BodyTransformEvent entity world
            world

        /// TODO: document this!
        static member initSpineSkeletonAnimationResult mapResult (entity : Entity) world =
            World.monitor (fun event world -> (Cascade, mapResult (FQueue.conj $ event.Data) world)) entity.SpineSkeletonAnimationTriggerEvent entity world

        /// Clear the current ImSim context.
        static member scopeWorld world =
            World.setContext Address.empty world

        /// Begin the ImSim declaration of a game with the given arguments.
        static member beginGame args (world : World) =
            if world.ContextImSim.Names.Length > 0 then raise (InvalidOperationException "ImSim game declared outside of valid ImSim context (must be called in World context).")
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName world.ContextImSim.Names)
            let world = World.setContext gameAddress world
            let game = Nu.Game gameAddress
            let (initializing, world) =
                match world.SimulantsImSim.TryGetValue game.GameAddress with
                | (true, gameImSim) -> (false, World.utilizeSimulantImSim game.GameAddress gameImSim world)
                | (false, _) ->
                    let world = World.addSimulantImSim game.GameAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = () } world
                    (true, world)
            let initializing = initializing || Reinitializing
            Seq.fold
                (fun world arg ->
                    if initializing || not arg.ArgStatic
                    then game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// End the ImSim declaration of a group with the given arguments.
        static member endGame (world : World) =
            match world.ContextImSim with
            | :? (Game Address) -> World.setContext Address.empty world
            | _ -> raise (InvalidOperationException "World.beginGame mismatch.")

        /// Make the game the current ImSim context.
        static member scopeGame (args : Game ArgImSim seq) world =
            let game = Game
            let world = World.setContext game.GameAddress world
            Seq.fold
                (fun world arg -> game.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c')
                world args

        (* NOTE: most of ImSim's screen functions are defined in WorldModule2.fs so they can access the internal screen transition APIs. *)

        /// Make a screen the current ImSim context.
        static member scopeScreen (screen : Screen) (args : Screen ArgImSim seq) world =
            let world = World.setContext screen.ScreenAddress world
            Seq.fold
                (fun world arg ->
                    if screen.GetExists world
                    then screen.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        static member private beginGroupPlus6<'d, 'r when 'd :> GroupDispatcher> (zero : 'r) init name groupFilePathOpt (args : Group ArgImSim seq) (world : World) : 'r * World =
            if world.ContextImSim.Names.Length < 2 then raise (InvalidOperationException "ImSim group declared outside of valid ImSim context (must be called in a Screen context).")
            let groupAddress = Address.makeFromArray (Array.add name world.ContextImSim.Names)
            let world = World.setContext groupAddress world
            let group = Nu.Group groupAddress
            let groupCreation = not (group.GetExists world)
            let (initializing, world) =
                match world.SimulantsImSim.TryGetValue group.GroupAddress with
                | (true, groupImSim) -> (false, World.utilizeSimulantImSim group.GroupAddress groupImSim world)
                | (false, _) ->

                    // init subscriptions _before_ potentially creating group
                    let world = World.addSimulantImSim group.GroupAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = () } world
                    let mapResult (mapper : 'r -> 'r) world =
                        let mapGroupImSim groupImSim = { groupImSim with Result = mapper (groupImSim.Result :?> 'r) }
                        World.tryMapSimulantImSim mapGroupImSim group.GroupAddress world
                    let world = init mapResult group world

                    // create group only when needed
                    let world =
                        if groupCreation then
                            match groupFilePathOpt with
                            | Some groupFilePath -> World.readGroupFromFile groupFilePath (Some name) group.Screen world |> snd
                            | None -> World.createGroup5 true typeof<'d>.Name (Some name) group.Screen world |> snd
                        else world

                    // protect group
                    let world = World.setGroupProtected true group world |> snd'

                    // fin
                    (true, world)

            let initializing = initializing || Reinitializing
            let world =
                Seq.fold
                    (fun world arg ->
                        if (initializing || not arg.ArgStatic) && group.GetExists world
                        then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                        else world)
                    world args
            let world =
                if groupCreation && group.GetExists world && WorldModule.UpdatingSimulants && World.getGroupSelected group world
                then WorldModule.tryProcessGroup true group world
                else world
            let result = match (World.getSimulantImSim group.GroupAddress world).Result with :? 'r as r -> r | _ -> zero
            let world = World.mapSimulantImSim (fun simulantImSim -> { simulantImSim with Result = zero }) group.GroupAddress world
            (result, world)

        static member inline private beginGroup4<'d> name groupFilePathOpt args world =
            World.beginGroupPlus6 () (fun _ _ world -> world) name groupFilePathOpt args world |> snd

        /// Begin the ImSim declaration of a group read from the given file path with the given arguments.
        /// Note that changing the file path over time has no effect as only the first moment is used.
        static member beginGroupFromFile (name : string) (groupFilePath : string) args (world : World) =
            if world.ContextImSim.Names.Length < 2 then raise (InvalidOperationException "ImSim group declared outside of valid ImSim context (must be called in a Screen context).")
            let groupAddress = Address.makeFromArray (Array.add name world.ContextImSim.Names)
            let world = World.setContext groupAddress world
            let group = Nu.Group groupAddress
            let world = // HACK: when group appears to exist as a placeholder created by Gaia, we destroy it so it can be made in a user-defined way.
                if  group.Name = "Scene" &&
                    group.GetExists world &&
                    Seq.isEmpty (World.getSovereignEntities group world) &&
                    getTypeName (group.GetDispatcher world) = nameof GroupDispatcher then
                    World.destroyGroupImmediate group world
                else world
            let groupCreation = not (group.GetExists world)
            let (initializing, world) =
                match world.SimulantsImSim.TryGetValue group.GroupAddress with
                | (true, groupImSim) ->
                    (false, World.utilizeSimulantImSim group.GroupAddress groupImSim world)
                | (false, _) ->
                    let world =
                        if groupCreation then
                            let groupDescriptorStr = File.ReadAllText groupFilePath
                            let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                            let world = World.readGroup groupDescriptor None group.Screen world |> snd
                            World.setGroupProtected true group world |> snd'
                        else world
                    let world = World.addSimulantImSim group.GroupAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = () } world
                    (true, world)
            let initializing = initializing || Reinitializing
            let world =
                Seq.fold
                    (fun world arg ->
                        if (initializing || not arg.ArgStatic) && group.GetExists world
                        then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                        else world)
                    world args
            let world =
                if groupCreation && group.GetExists world && WorldModule.UpdatingSimulants && World.getGroupSelected group world
                then WorldModule.tryProcessGroup true group world
                else world
            world

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

        /// Make a group the current ImSim context.
        static member scopeGroup (group : Group) (args : Group ArgImSim seq) world =
            let world = World.setContext group.GroupAddress world
            Seq.fold
                (fun world arg ->
                    if group.GetExists world
                    then group.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// Begin the ImSim declaration of an entity with the given arguments.
        static member beginEntityPlus<'d, 'r when 'd :> EntityDispatcher> (zero : 'r) init name (args : Entity ArgImSim seq) (world : World) : 'r * World =
            if world.ContextImSim.Names.Length < 3 then raise (InvalidOperationException "ImSim entity declared outside of valid ImSim context (must be called in either Group or Entity context).")
            let entityAddress = Address.makeFromArray (Array.add name world.ContextImSim.Names)
            let world = World.setContext entityAddress world
            let entity = Nu.Entity entityAddress
            let entityCreation = not (entity.GetExists world)
            let (initializing, world) =
                match world.SimulantsImSim.TryGetValue entity.EntityAddress with
                | (true, entityImSim) -> (false, World.utilizeSimulantImSim entity.EntityAddress entityImSim world)
                | (false, _) ->

                    // init subscriptions _before_ potentially creating entity
                    let world =
                        let world = World.addSimulantImSim entity.EntityAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = zero } world
                        let mapResult (mapper : 'r -> 'r) world =
                            let mapEntityImSim entityImSim = { entityImSim with Result = mapper (entityImSim.Result :?> 'r) }
                            World.tryMapSimulantImSim mapEntityImSim entity.EntityAddress world
                        init mapResult entity world

                    // create entity only when needed
                    let world =
                        if entityCreation
                        then World.createEntity6 true typeof<'d>.Name OverlayNameDescriptor.DefaultOverlay (Some entity.Surnames) entity.Group world |> snd
                        else world

                    // protect entity
                    let world = World.setEntityProtected true entity world |> snd'

                    // fin
                    (true, world)

            let initializing = initializing || Reinitializing
            let mutable mountArgApplied = false
            let world =
                Seq.fold
                    (fun world arg ->
                        if (initializing || not arg.ArgStatic) && entity.GetExists world then
                            mountArgApplied <- mountArgApplied || arg.ArgLens.Name = Constants.Engine.MountOptPropertyName
                            entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                        else world)
                    world args
            let world =
                if initializing && not mountArgApplied && entity.GetExists world && entity.Surnames.Length > 1
                then entity.SetMountOpt (Some (Relation.makeParent ())) world
                else world
            let world =
                if entityCreation && entity.GetExists world && WorldModule.UpdatingSimulants && World.getEntitySelected entity world
                then WorldModule.tryProcessEntity true entity world
                else world
            let result = match (World.getSimulantImSim entity.EntityAddress world).Result with :? 'r as r -> r | _ -> zero
            let world = World.mapSimulantImSim (fun simulantImSim -> { simulantImSim with Result = zero }) entity.EntityAddress world
            (result, world)

        /// Begin the ImSim declaration of an entity with the given arguments.
        static member beginEntity<'d when 'd :> EntityDispatcher> name args world =
            World.beginEntityPlus<'d, unit> () (fun _ _ world -> world) name args world |> snd

        static member internal beginEntityFromDescriptor entityDescriptor (world : World) : bool * Entity * World =
            if world.ContextImSim.Names.Length < 3 then raise (InvalidOperationException "ImSim entity declared outside of valid ImSim context (must be called in either Group or Entity context).")
            let entityName = match EntityDescriptor.getNameOpt entityDescriptor with Some name -> name | None -> "Entity"
            let entityAddress = Address.makeFromArray (Array.add entityName world.ContextImSim.Names)
            let world = World.setContext entityAddress world
            let entity = Nu.Entity entityAddress
            let world =
                if not (entity.GetExists world) then
                    let world = World.readEntity false true entityDescriptor None entity.Group world |> snd
                    World.setEntityProtected true entity world |> snd'
                else world
            match world.SimulantsImSim.TryGetValue entity.EntityAddress with
            | (true, entityImSim) -> (false, entity, World.utilizeSimulantImSim entity.EntityAddress entityImSim world)
            | (false, _) -> (true, entity, World.addSimulantImSim entity.EntityAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = () } world)

        /// Begin the ImSim declaration of a group read from the given file path with the given arguments.
        /// Note that changing the file path over time has no effect as only the first moment is used.
        static member internal beginEntityFromFile entityFilePath args world =
            let entityDescriptorStr = File.ReadAllText entityFilePath
            let entityDescriptor = scvalue<EntityDescriptor> entityDescriptorStr
            let (initializing, entity, world) = World.beginEntityFromDescriptor entityDescriptor world
            let initializing = initializing || Reinitializing
            Seq.fold
                (fun world arg ->
                    if (initializing || not arg.ArgStatic) && entity.GetExists world
                    then entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

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

        /// ImSim declare an entity with the given arguments.
        static member doEntityPlus<'d, 'r when 'd :> EntityDispatcher> zero init name args world =
            let (result, world) = World.beginEntityPlus<'d, 'r> zero init name args world
            let world = World.endEntity world
            (result, world)

        /// ImSim declare an entity with the given arguments.
        static member doEntity<'d when 'd :> EntityDispatcher> name args world =
            let world = World.beginEntity<'d> name args world
            World.endEntity world

        /// Make an entity the current ImSim context.
        static member scopeEntity (entity : Entity) (args : Entity ArgImSim seq) world =
            let world = World.setContext entity.EntityAddress world
            Seq.fold
                (fun world arg ->
                    if entity.GetExists world
                    then entity.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> __c'
                    else world)
                world args

        /// Begin the ImSim declaration of associated gui entities with the given arguments.
        static member beginAssociation name args world = World.beginEntity<GuiDispatcher> name args world

        /// End the ImSim declaration of associated gui entities.
        static member endAssociation world = World.endEntity world

        /// ImSim declare an empty association of gui entities with the given arguments.
        static member doAssociation name args world = World.doEntity<GuiDispatcher> name args world

        /// ImSim declare a basic static sprite emitter with the given arguments.
        static member doBasicStaticSpriteEmitter name args world = World.doEntity<BasicStaticSpriteEmitterDispatcher> name args world

        /// ImSim declare a 2d effect with the given arguments.
        static member doEffect2d name args world = World.doEntity<Effect2dDispatcher> name args world

        /// ImSim declare a static sprite with the given arguments.
        static member doStaticSprite name args world = World.doEntity<StaticSpriteDispatcher> name args world

        /// ImSim declare an animated sprite with the given arguments.
        static member doAnimatedSprite name args world = World.doEntity<AnimatedSpriteDispatcher> name args world

        /// ImSim declare a text entity with the given arguments.
        static member doText name args world = World.doEntity<TextDispatcher> name args world

        /// ImSim declare a label with the given arguments.
        static member doLabel name args world = World.doEntity<LabelDispatcher> name args world

        /// ImSim declare a button with the given arguments.
        static member doButton name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun _ world -> (Cascade, mapResult tautology world)) entity.ClickEvent entity world
            World.doEntityPlus<ButtonDispatcher, _> false init name args world

        /// ImSim declare a toggle button with the given arguments.
        static member doToggleButton name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun _ world -> (Cascade, mapResult tautology world)) entity.ToggleEvent entity world
            let (toggleChanged, world) = World.doEntityPlus<ToggleButtonDispatcher, _> false init name args world
            (world.DeclaredEntity.GetToggled world, toggleChanged, world)

        /// ImSim declare a radio button with the given arguments.
        static member doRadioButton name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun _ world -> (Cascade, mapResult tautology world)) entity.DialEvent entity world
            let (dialChanged, world) = World.doEntityPlus<RadioButtonDispatcher, _> false init name args world
            (world.DeclaredEntity.GetDialed world, dialChanged, world)

        /// ImSim declare a fill bar with the given arguments.
        static member doFillBar name args world = World.doEntity<FillBarDispatcher> name args world

        /// ImSim declare a feeler with the given arguments.
        static member doFeeler name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun _ world -> (Cascade, mapResult tautology world)) entity.TouchEvent entity world
            let (touchChanged, world) = World.doEntityPlus<FeelerDispatcher, _> false init name args world
            (world.DeclaredEntity.GetTouched world, touchChanged, world)

        /// ImSim declare a text box entity with the given arguments.
        static member doTextBox name args world =
            let init mapResult (entity : Entity) world = World.monitor (fun _ world -> (Cascade, mapResult tautology world)) entity.TextEditEvent entity world
            let (textChanged, world) = World.doEntityPlus<TextBoxDispatcher, _> false init name args world
            (world.DeclaredEntity.GetText world, textChanged, world)

        /// ImSim declare an fps entity with the given arguments.
        static member doFps name args world = World.doEntity<FpsDispatcher> name args world

        /// ImSim declare the beginning of a panel with the given arguments.
        static member beginPanel name args world = World.beginEntity<PanelDispatcher> name args world

        /// ImSim declare the end of a panel.
        static member endPanel world = World.endEntity world

        /// ImSim declare a panel with the given arguments.
        static member doPanel name args world = World.doEntity<PanelDispatcher> name args world

        /// ImSim declare a 2d block with the given arguments.
        static member doBlock2d name args world =
            let (results, world) = World.doEntityPlus<Block2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 2d box with the given arguments.
        static member doBox2d name args world =
            let (results, world) = World.doEntityPlus<Box2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 2d sphere with the given arguments.
        static member doSphere2d name args world =
            let (results, world) = World.doEntityPlus<Sphere2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 2d ball with the given arguments.
        static member doBall2d name args world =
            let (results, world) = World.doEntityPlus<Ball2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 2d character with the given arguments.
        static member doCharacter2d name args world =
            let (results, world) = World.doEntityPlus<Character2dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a tile map with the given arguments.
        static member doTileMap name args world =
            let (results, world) = World.doEntityPlus<TileMapDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a user-defined tile map with the given arguments.
        static member doTmxMap name args world =
            let (results, world) = World.doEntityPlus<TmxMapDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a tile map with the given arguments.
        static member doSpineSkeleton name args world =
            let (results, world) = World.doEntityPlus<SpineSkeletonDispatcher, _> FQueue.empty World.initSpineSkeletonAnimationResult name args world
            (results, world)

        /// ImSim declare a 3d light probe with the given arguments.
        static member doLightProbe3d name args world = World.doEntity<LightProbe3dDispatcher> name args world

        /// ImSim declare a 3d light with the given arguments.
        static member doLight3d name args world = World.doEntity<Light3dDispatcher> name args world

        /// ImSim declare a sky box with the given arguments.
        static member doSkyBox name args world = World.doEntity<SkyBoxDispatcher> name args world

        /// ImSim declare a basic static billboard emitter with the given arguments.
        static member doBasicStaticBillboardEmitter name args world = World.doEntity<BasicStaticBillboardEmitterDispatcher> name args world

        /// ImSim declare a 3d effect with the given arguments.
        static member doEffect3d name args world = World.doEntity<Effect3dDispatcher> name args world

        /// ImSim declare a 3d block with the given arguments.
        static member doBlock3d name args world =
            let (results, world) = World.doEntityPlus<Block3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 3d box with the given arguments.
        static member doBox3d name args world =
            let (results, world) = World.doEntityPlus<Box3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 3d sphere with the given arguments.
        static member doSphere3d name args world =
            let (results, world) = World.doEntityPlus<Sphere3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 3d ball with the given arguments.
        static member doBall3d name args world =
            let (results, world) = World.doEntityPlus<Ball3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a static billboard with the given arguments.
        static member doStaticBillboard name args world = World.doEntity<StaticBillboardDispatcher> name args world

        /// ImSim declare an animated billboard with the given arguments.
        static member doAnimatedBillboard name args world = World.doEntity<AnimatedBillboardDispatcher> name args world

        /// ImSim declare a static model with the given arguments.
        static member doStaticModel name args world = World.doEntity<StaticModelDispatcher> name args world

        /// ImSim declare a animated model with the given arguments.
        static member doAnimatedModel name args world = World.doEntity<AnimatedModelDispatcher> name args world

        /// ImSim declare a sensor model with the given arguments.
        static member doSensorModel name args world =
            let (results, world) = World.doEntityPlus<SensorModelDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a rigid model with the given arguments.
        static member doRigidModel name args world =
            let (results, world) = World.doEntityPlus<RigidModelDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a static model surface with the given arguments.
        static member doStaticModelSurface name args world = World.doEntity<StaticModelSurfaceDispatcher> name args world

        /// ImSim declare a sensor model surface with the given arguments.
        static member doSensorModelSurface name args world =
            let (results, world) = World.doEntityPlus<SensorModelSurfaceDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a rigid model surface with the given arguments.
        static member doRigidModelSurface name args world =
            let (results, world) = World.doEntityPlus<RigidModelSurfaceDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 3d character with the given arguments.
        static member doCharacter3d name args world =
            let (results, world) = World.doEntityPlus<Character3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 3d body joint with the given arguments.
        static member doBodyJoint3d name args world =
            let (results, world) = World.doEntityPlus<BodyJoint3dDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyJointId world, results, world)

        /// ImSim declare a terrain with the given arguments.
        static member doTerrain name args world =
            let (results, world) = World.doEntityPlus<TerrainDispatcher, _> FQueue.empty World.initBodyResult name args world
            (world.DeclaredEntity.GetBodyId world, results, world)

        /// ImSim declare a 3d nav config with the given arguments.
        static member doNav3dConfig name args world = World.doEntity<Nav3dConfigDispatcher> name args world

        /// ImSim declare a 3d light config with the given arguments.
        static member doLighting3dConfig name args world = World.doEntity<Lighting3dConfigDispatcher> name args world

        /// ImSim declare a static model hierarchy with the given arguments.
        static member doStaticModelHierarchy name args world = World.doEntity<StaticModelHierarchyDispatcher> name args world

        /// ImSim declare a rigid model hierarchy with the given arguments.
        static member doRigidModelHierarchy name args world = World.doEntity<RigidModelHierarchyDispatcher> name args world