// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What's being read - its lines and the page of them showing, beside the intent it's given - and its page turned,
/// raised for the gameplay screen to turn.
[<AutoOpen>]
module ReadingDispatcherExtensions =
    type Entity with
        member this.GetReading world : Reading = this.Get (nameof this.Reading) world
        member this.SetReading (value : Reading) world = this.Set (nameof this.Reading) value world
        member this.Reading = lens (nameof this.Reading) this this.GetReading this.SetReading
        member this.PageEvent = Events.PageEvent --> this

/// A line being read, in a box low on the screen, a page at a time.
type ReadingDispatcher () =
    inherit GuiDispatcherImSim ()

    static member Properties =
        [define Entity.Reading { Lines = []; Page = 0; After = Fin; ActedOnOpt = None }
         define Entity.Intent Intent.empty]

    override this.Process (entity, world) =

        // turn the page on acting
        if world.TimeAdvancing && (entity.GetIntent world).Act then World.publish () entity.PageEvent entity world

        // the page showing
        let reading = entity.GetReading world
        if reading.Page < reading.Lines.Length then
            World.doText "ReadingBox"
                [Entity.Position .= v3 0.0f -130.0f 0.0f
                 Entity.Size .= v3 600.0f 56.0f 0.0f
                 Entity.Text @= reading.Lines[reading.Page]
                 Entity.TextColor .= Color.White
                 Entity.BackdropImageOpt .= Some Assets.Default.Black
                 Entity.Color .= Color.White.WithA 0.78f] world
