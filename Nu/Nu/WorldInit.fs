// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Reflection
open Prime
open Nu

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Nu =

    let mutable private Initialized = false
    let private LoadedAssemblies = Dictionary<string, Assembly> HashIdentity.Structural

    /// Initialize the game engine.
    let init sync =

        // init only if needed
        if not Initialized then

            // make types load reflectively from pathed (non-static) assemblies
            AppDomain.CurrentDomain.AssemblyLoad.Add
                (fun args -> LoadedAssemblies.[args.LoadedAssembly.FullName] <- args.LoadedAssembly)
            AppDomain.CurrentDomain.add_AssemblyResolve ^ ResolveEventHandler
                (fun _ args -> snd ^ LoadedAssemblies.TryGetValue args.Name)

            // ensure the current culture is invariate
            System.Threading.Thread.CurrentThread.CurrentCulture <-
                System.Globalization.CultureInfo.InvariantCulture

            // init logging
            Log.init ^ Some "Log.txt"

            // initialize math module
            Math.init ()

            // init F# reach-arounds
            WorldModule.eval <- fun expr (script : Script) simulant world ->
                ignore script // TODO: utilize script bindings
                let env = Env.make World.choose false (dictPlus []) simulant world
                let (_, env) = ScriptSystem.run expr env
                Env.getWorld env
#if DEBUG
            Debug.World.viewGame <- fun world -> Debug.Game.view (world :?> World)
            Debug.World.viewScreen <- fun screen world -> Debug.Screen.view (screen :?> Screen) (world :?> World)
            Debug.World.viewLayer <- fun layer world -> Debug.Layer.view (layer :?> Layer) (world :?> World)
            Debug.World.viewEntity <- fun entity world -> Debug.Entity.view (entity :?> Entity) (world :?> World)
#endif

            // init Vsync with incoming parameter
            Vsync.init sync

            // init event world caching
            EventWorld.setEventAddressCaching true

            // mark init flag
            Initialized <- true