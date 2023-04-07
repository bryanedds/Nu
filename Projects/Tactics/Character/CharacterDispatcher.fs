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
            let renderMaterial =
                { AlbedoOpt = ValueSome Color.White
                  MetalnessOpt = ValueSome 0.0f
                  RoughnessOpt = ValueSome 1.25f
                  EmissionOpt = ValueSome 1.0f
                  AmbientOcclusionOpt = ValueSome 1.0f }
            let albedoImage = asset "Field" "Jinn"
            let inset = getSpriteInset character world
            let characterView =
                Render3d (
                    RenderBillboard
                        (transform.Absolute, transform.AffineMatrix, ValueSome inset, renderMaterial,
                         albedoImage, Assets.Default.MaterialMetalness, Assets.Default.MaterialRoughness, Assets.Default.MaterialRoughness, Assets.Default.MaterialEmission, albedoImage,
                         ValueSome OpenGL.TextureMinFilter.NearestMipmapNearest, ValueSome OpenGL.TextureMagFilter.Nearest, DeferredRenderType))
            characterView

        override this.GetQuickSize (_, _) =
            v3 1.0f 2.0f 1.0f

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
            Some
                (box3
                    (bounds.Min + bounds.Size * v3 0.0f 0.5f 0.0f)
                    (bounds.Size * v3 1.0f 0.5f 1.0f))