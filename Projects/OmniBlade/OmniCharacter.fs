namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniCharacter =

    type [<NoComparison>] CharacterModel =
        { CharacterPosition : Vector2
          CharacterSize : Vector2
          CharacterAnimationSheet : Image AssetTag
          CharacterAnimationState : CharacterAnimationState
          CharacterState : CharacterState
          ActionTime : int }

    type Entity with
    
        member this.GetCharacterAnimationSheet world : Image AssetTag = this.Get Property? CharacterAnimationSheet world
        member this.SetCharacterAnimationSheet (value : Image AssetTag) world = this.Set Property? CharacterAnimationSheet value world
        member this.CharacterAnimationSheet = Lens.make Property? CharacterAnimationSheet this.GetCharacterAnimationSheet this.SetCharacterAnimationSheet this
        member this.GetCharacterAnimationState world : CharacterAnimationState = this.Get Property? CharacterAnimationState world
        member this.SetCharacterAnimationState (value : CharacterAnimationState) world = this.Set Property? CharacterAnimationState value world
        member this.CharacterAnimationState = Lens.make Property? CharacterAnimationState this.GetCharacterAnimationState this.SetCharacterAnimationState this
        member this.GetCharacterState world : CharacterState = this.Get Property? CharacterState world
        member this.SetCharacterState (value : CharacterState) world = this.Set Property? CharacterState value world
        member this.CharacterState = Lens.make Property? CharacterState this.GetCharacterState this.SetCharacterState this
        member this.GetActionTimeNp world : int = this.Get Property? ActionTimeNp world
        member this.SetActionTimeNp (value : int) world = this.Set Property? ActionTimeNp value world
        member this.ActionTimeNp = Lens.make Property? ActionTimeNp this.GetActionTimeNp this.SetActionTimeNp this

    type CharacterDispatcher () =
        inherit EntityDispatcher ()
        
        static let [<Literal>] CelSize = 160.0f

        static let getSpriteInset (entity : Entity) world =
            let characterAnimationState = entity.GetCharacterAnimationState world
            let index = CharacterAnimationState.index (World.getTickTime world) characterAnimationState
            let offset = Vector2 (single index.X * CelSize, single index.Y * CelSize)
            let inset = Vector4 (offset.X, offset.Y, offset.X + CelSize, offset.Y + CelSize)
            inset

        static let getSpriteColor (entity : Entity) world =
            let statuses = (entity.GetCharacterState world).Statuses
            let color =
                let state = entity.GetCharacterAnimationState world
                if state.CharacterAnimationCycle = CharacterAnimationCycle.WoundCycle && (entity.GetCharacterState world).IsEnemy then
                    match CharacterAnimationState.progressOpt (World.getTickTime world) state with
                    | Some progress -> Vector4 (1.0f,0.5f,1.0f,1.0f-progress) // purple
                    | None -> failwithumf ()
                elif Set.contains PoisonStatus statuses then Vector4 (0.5f,1.0f,0.5f,1.0f) // green
                elif Set.contains MuteStatus statuses then Vector4 (0.1f,1.0f,0.5f,1.0f) // orange
                elif Set.contains SleepStatus statuses then Vector4 (0.5f,0.5f,1.0f,1.0f) // blue
                else Vector4.One
            color

        static member Properties =
            [define Entity.CharacterAnimationSheet Assets.JinnAnimationSheet
             define Entity.CharacterAnimationState { TimeStart = 0L; CharacterAnimationCycle = ReadyCycle; Direction = Downward; Stutter = 10 }
             define Entity.CharacterState CharacterState.empty
             define Entity.ActionTimeNp 0
             define Entity.Omnipresent true
             define Entity.PublishChanges true]

        override this.Actualize (entity, world) =
            if entity.GetInView world then
                World.enqueueRenderMessage
                    (RenderDescriptorMessage
                        (LayerableDescriptor
                            { Depth = entity.GetDepth world
                              PositionY = (entity.GetPosition world).Y
                              AssetTag = entity.GetCharacterAnimationSheet world
                              LayeredDescriptor =
                              SpriteDescriptor
                                { Position = entity.GetPosition world
                                  Size = entity.GetSize world
                                  Rotation = entity.GetRotation world
                                  Offset = Vector2.Zero
                                  ViewType = entity.GetViewType world
                                  InsetOpt = Some (getSpriteInset entity world)
                                  Image = entity.GetCharacterAnimationSheet world
                                  Color = getSpriteColor entity world
                                  Flip = FlipNone }}))
                    world
            else world