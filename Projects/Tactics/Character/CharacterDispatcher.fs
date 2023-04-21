namespace Tactics
open System
open Prime
open Nu
open Nu.Declarative
open Tactics

[<AutoOpen>]
module CharacterDispatcher =

    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()

    type CharacterDispatcher () =
        inherit EntityDispatcher3d<Character, Message, Command> (true, false, Character.empty)

        static let getSpriteInset (character : Character) world =
            Character.getAnimationInset (World.getUpdateTime world) character

        override this.Initialize (_, _) =
            [Entity.Presence == Omnipresent]

        override this.View (character, entity, world) =
            let mutable transform = entity.GetTransform world
            let properties =
                { AlbedoOpt = Some Color.White
                  MetallicOpt = Some 0.0f
                  RoughnessOpt = Some 0.5f
                  AmbientOcclusionOpt = Some 1.0f
                  EmissionOpt = Some 1.0f
                  HeightOpt = Some 0.0f
                  InvertRoughnessOpt = Some false }
            let albedoImage = asset "Field" "Jinn"
            let inset = getSpriteInset character world
            let characterView =
                Render3d
                    (RenderBillboard
                        { Absolute = transform.Absolute
                          ModelMatrix = transform.AffineMatrix
                          InsetOpt = Some inset
                          SurfaceProperties = properties
                          AlbedoImage = albedoImage
                          MetallicImage = Assets.Default.MaterialMetallic
                          RoughnessImage = Assets.Default.MaterialRoughness
                          AmbientOcclusionImage = Assets.Default.MaterialRoughness
                          EmissionImage = Assets.Default.MaterialEmission
                          NormalImage = albedoImage
                          HeightImage = Assets.Default.MaterialHeight
                          MinFilterOpt = Some OpenGL.TextureMinFilter.NearestMipmapNearest
                          MagFilterOpt = Some OpenGL.TextureMagFilter.Nearest
                          RenderType = DeferredRenderType })
            characterView

        override this.GetQuickSize (_, _) =
            v3 1.0f 1.0f 1.0f

        override this.RayCast (ray, entity, world) =
            // TODO: intersect against oriented quad rather than box.
            match this.TryGetHighlightBounds (entity, world) with
            | Some bounds ->
                let intersectionOpt = ray.Intersects bounds
                if intersectionOpt.HasValue then [|intersectionOpt.Value|]
                else [||]
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            let bounds = entity.GetBounds world
            Some (box3 bounds.Min bounds.Size)