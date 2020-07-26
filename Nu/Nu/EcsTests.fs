// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module EcsTests =

    type [<NoEquality; NoComparison; Struct>] private TransformIntersection =
        { mutable RefCount : int
          Transform : Transform ComponentRef }
        interface Component with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value

    type private ExampleJunctioned () =
        inherit SystemJunctioned<TransformIntersection, World>
            [|typeof<Transform>.Name|]

        override this.Junction junctions entityId _ ecs =
            { RefCount = 0
              Transform = ecs.Junction<Transform> junctions entityId }

        override this.Disjunction junctions entityId _ ecs =
            ecs.Disjunction<Transform> junctions entityId

        override this.ProcessUpdate _ world =
            for i = 0 to this.Components.Length - 1 do
                let comp = &this.Components.[i]
                let transform = &comp.Transform.Value
                transform.Enabled <- false
                transform.Absolute <- false
            world