namespace OpenGL
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SurfaceBatch =

    type [<NoEquality; NoComparison; Struct>] private State =
        { PhysicallyBasedSurface : OpenGL.Hl.PhysicallyBasedSurface
          AlbedoMap : uint
          MetalnessMap : uint
          RoughnessMap : uint
          NormalMap : uint
          LightPositions : Vector3 array
          LightColors : Vector3 array }

        static member inline changed state state2 =
            state.PhysicallyBasedSurface.PhysicallyBasedVao <> state2.PhysicallyBasedSurface.PhysicallyBasedVao &&
            state.AlbedoMap <> state2.AlbedoMap &&
            state.MetalnessMap <> state2.MetalnessMap &&
            state.RoughnessMap <> state2.RoughnessMap &&
            state.NormalMap <> state2.NormalMap &&
            state.LightPositions <> state2.LightPositions &&
            state.LightColors <> state2.LightColors

        static member create surface albedoMap metalnessMap roughnessMap normalMap lightPositions lightColors =
            { PhysicallyBasedSurface = surface
              AlbedoMap = albedoMap
              MetalnessMap = metalnessMap
              RoughnessMap = roughnessMap
              NormalMap = normalMap
              LightPositions = lightPositions
              LightColors = lightColors }

        static member defaultState =
            State.create Unchecked.defaultof<OpenGL.Hl.PhysicallyBasedSurface> 0u 0u 0u 0u [||] [||]

