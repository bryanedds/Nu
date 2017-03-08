// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open System.Reflection
open Prime
open Nu

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Nu =

    let mutable private Initialized = false
    let private LoadedAssemblies = Dictionary<string, Assembly> () (* HashIdentity *)

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

            // init eval F# reach-arounds
            // TODO: remove duplicated code with the following 4 functions...
            WorldModule.eval <- fun expr localFrame scriptContext world ->
                let oldLocalFrame = World.getLocalFrame world
                let oldScriptContext = World.getScriptContext world
                let world = World.setLocalFrame localFrame world
                let world = World.setScriptContext scriptContext world
                let (evaled, world) = World.evalInternal expr world
                let world = World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                (evaled, world)

            // init evalMany F# reach-around
            WorldModule.evalMany <- fun exprs localFrame scriptContext world ->
                let oldLocalFrame = World.getLocalFrame world
                let oldScriptContext = World.getScriptContext world
                let world = World.setLocalFrame localFrame world
                let world = World.setScriptContext scriptContext world
                let (evaleds, world) = World.evalManyInternal exprs world
                let world = World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                (evaleds, world)

            // init evalWithLogging F# reach-arounds
            WorldModule.evalWithLogging <- fun expr localFrame scriptContext world ->
                let oldLocalFrame = World.getLocalFrame world
                let oldScriptContext = World.getScriptContext world
                let world = World.setLocalFrame localFrame world
                let world = World.setScriptContext scriptContext world
                let (evaled, world) = World.evalWithLoggingInternal expr world
                let world = World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                (evaled, world)

            // init evalMany F# reach-around
            WorldModule.evalManyWithLogging <- fun exprs localFrame scriptContext world ->
                let oldLocalFrame = World.getLocalFrame world
                let oldScriptContext = World.getScriptContext world
                let world = World.setLocalFrame localFrame world
                let world = World.setScriptContext scriptContext world
                let (evaleds, world) = World.evalManyWithLoggingInternal exprs world
                let world = World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                (evaleds, world)

            // init debug view F# reach-arounds
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