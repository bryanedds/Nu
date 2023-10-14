namespace Tactics
open System
open Prime
open Nu

[<AutoOpen>]
module CharacterDispatcher =

    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()

    type CharacterDispatcher () =
        inherit EntityDispatcher3d<Character, Message, Command> (true, false, Character.empty)

        static let getSpriteInset (character : Character) (world : World) =
            Character.getAnimationInset world.UpdateTime character

        override this.Initialize (_, _) =
            [Entity.Presence == Omnipresent]

        override this.View (character, entity, world) =
            let mutable transform = entity.GetTransform world
            let properties =
                { AlbedoOpt = ValueSome Color.White
                  MetallicOpt = ValueSome 0.0f
                  RoughnessOpt = ValueSome 0.8f
                  AmbientOcclusionOpt = ValueSome 1.0f
                  EmissionOpt = ValueSome 0.0f
                  HeightOpt = ValueSome 0.0f
                  InvertRoughnessOpt = ValueSome false }
            let albedoImage = asset "Field" "Jinn"
            let inset = getSpriteInset character world
            let characterView =
                Render3d
                    (RenderBillboard
                        { Absolute = transform.Absolute
                          ModelMatrix = transform.AffineMatrixOffset
                          InsetOpt = Some inset
                          MaterialProperties = properties
                          AlbedoImage = albedoImage
                          MetallicImage = Assets.Default.MaterialMetallic
                          RoughnessImage = Assets.Default.MaterialRoughness
                          AmbientOcclusionImage = Assets.Default.MaterialRoughness
                          EmissionImage = Assets.Default.MaterialEmission
                          NormalImage = Assets.Default.MaterialNormal
                          HeightImage = Assets.Default.MaterialHeight
                          MinFilterOpt = Some OpenGL.TextureMinFilter.NearestMipmapNearest
                          MagFilterOpt = Some OpenGL.TextureMagFilter.Nearest
                          RenderType = ForwardRenderType (0.0f, 0.0f) })
            characterView

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