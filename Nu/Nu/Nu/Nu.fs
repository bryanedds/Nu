// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.ComponentModel
open System.Collections.Generic
open System.Reflection
open Prime
open Nu

[<RequireQualifiedAccess>]
module Nu =

    let private LoadedAssemblies = Dictionary<string, Assembly> HashIdentity.Structural

    /// Initialize the Nu game engine. Basically calls all the unavoidable imperative stuff
    /// needed to set up the .NET environment appropriately. MUST be called before making the
    /// world.
    let init () =

        // make types load reflectively from pathed (non-static) assemblies
        AppDomain.CurrentDomain.AssemblyLoad.Add
            (fun args -> LoadedAssemblies.[args.LoadedAssembly.FullName] <- args.LoadedAssembly)
        AppDomain.CurrentDomain.add_AssemblyResolve ^ ResolveEventHandler
            (fun _ args -> snd ^ LoadedAssemblies.TryGetValue args.Name)

        // ensure the current culture is invariate
        System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture

        // init logging
        Log.init ^ Some "Log.txt"

        // init type converters
        Math.initTypeConverters ()

        // init F# reach-arounds
        World.rebuildEntityTree <- fun screen world ->
            let tree = QuadTree.make Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
            let entities = screen |> flip World.proxyGroups world |> Seq.map (flip World.proxyEntities world) |> Seq.concat
            for entity in entities do
                let entityMaxBounds = World.getEntityMaxBounds entity world
                QuadTree.addElement false entityMaxBounds entity tree
            (world, tree)