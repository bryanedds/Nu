module NuEditLogic.Entity
open System
open System.Reflection
open Nu.Core
open Nu.Entity
open Nu.Simulation

type EntityFieldChange =
    { Address : Address
      FieldInfo : FieldInfo
      Value : obj }

let getEntityTypes entity =
    match entity.EntitySemantic with
    | Gui gui ->
        let guiSemType =
            match gui.GuiSemantic with
            | Button _ -> typeof<Button>
            | Label _ -> typeof<Label>
            | TextBox _ -> typeof<TextBox>
            | Toggle _ -> typeof<Toggle>
            | Feeler _ -> typeof<Feeler>
        [typeof<Entity>; typeof<Gui>; guiSemType]
    | Actor actor ->
        let actorSemType =
            match actor.ActorSemantic with
            | Block _ -> typeof<Block>
            | Avatar _ -> typeof<Avatar>
            | TileMap _ -> typeof<TileMap>
        [typeof<Entity>; typeof<Actor>; actorSemType]

let getEntityFieldValue (fieldInfo : FieldInfo) entity =
    if entity.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) = fieldInfo
    then fieldInfo.GetValue entity
    else
        match entity.EntitySemantic with
        | Gui gui ->
            if gui.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) = fieldInfo
            then fieldInfo.GetValue gui
            else
                match gui.GuiSemantic with
                | Button button -> fieldInfo.GetValue button
                | Label label -> fieldInfo.GetValue label
                | TextBox textBox -> fieldInfo.GetValue textBox
                | Toggle toggle -> fieldInfo.GetValue toggle
                | Feeler feeler -> fieldInfo.GetValue feeler
        | Actor actor ->
            if actor.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) = fieldInfo
            then fieldInfo.GetValue actor
            else
                match actor.ActorSemantic with
                | Block block -> fieldInfo.GetValue block
                | Avatar avatar -> fieldInfo.GetValue avatar
                | TileMap tileMap -> fieldInfo.GetValue tileMap

let setEntityFieldValue world (change : EntityFieldChange) =
    let entityLens = World.entity change.Address
    let entity = get world entityLens
    let fieldInfo = change.FieldInfo
    let value = change.Value
    let entity_ =
        let entity_ = { entity with Id = entity.Id } // NOTE: this is just a hacky way to copy an entity in lieu of reflection
        if entity_.GetType(). GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) = fieldInfo
        then let _ = fieldInfo.SetValue (entity_, value) in entity_
        else
            match entity.EntitySemantic with
            | Gui gui ->
                let gui_ = { gui with Position = gui.Position } // NOTE: hacky copy
                if gui_.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) = fieldInfo
                then let _ = fieldInfo.SetValue (gui_, value) in { entity with EntitySemantic = Gui gui_ }
                else
                    match gui.GuiSemantic with
                    | Button button ->
                        let button_ = { button with IsDown = button.IsDown } // NOTE: hacky copy
                        fieldInfo.SetValue (button_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = Button button_ }}
                    | Label label ->
                        let label_ = { label with LabelSprite = label.LabelSprite } // NOTE: hacky copy
                        fieldInfo.SetValue (label_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = Label label_ }}
                    | TextBox textBox ->
                        let textBox_ = { textBox with BoxSprite = textBox.BoxSprite } // NOTE: hacky copy
                        fieldInfo.SetValue (textBox_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = TextBox textBox_ }}
                    | Toggle toggle ->
                        let toggle_ = { toggle with IsPressed = toggle.IsPressed } // NOTE: hacky copy
                        fieldInfo.SetValue (toggle_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = Toggle toggle_ }}
                    | Feeler feeler ->
                        let feeler_ = { feeler with IsTouched = feeler.IsTouched } // NOTE: hacky copy
                        fieldInfo.SetValue (feeler_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = Feeler feeler_ }}
            | Actor actor ->
                let actor_ = { actor with Position = actor.Position } // NOTE: hacky copy
                if actor_.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) = fieldInfo
                then let _ = fieldInfo.SetValue (actor_, value) in { entity with EntitySemantic = Actor actor_ }
                else
                    match actor.ActorSemantic with
                    | Block block ->
                        let block_ = { block with PhysicsId = block.PhysicsId } // NOTE: hacky copy
                        fieldInfo.SetValue (block_, value)
                        { entity with EntitySemantic = Actor { actor with ActorSemantic = Block block_ }}
                    | Avatar avatar ->
                        let avatar_ = { avatar with PhysicsId = avatar.PhysicsId } // NOTE: hacky copy
                        fieldInfo.SetValue (avatar_, value)
                        { entity with EntitySemantic = Actor { actor with ActorSemantic = Avatar avatar_ }}
                    | TileMap tileMap ->
                        let tileMap_ = { tileMap with PhysicsIds = tileMap.PhysicsIds } // NOTE: hacky copy
                        fieldInfo.SetValue (tileMap_, value)
                        { entity with EntitySemantic = Actor { actor with ActorSemantic = TileMap tileMap_ }}
    set entity_ world entityLens

let setEntityFieldValues changes world =
    Seq.fold setEntityFieldValue world changes