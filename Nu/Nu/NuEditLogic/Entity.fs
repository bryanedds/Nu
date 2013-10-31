module NuEditLogic.Entity
open System
open System.Reflection
open Nu.Core
open Nu.Entity
open Nu.Simulation

type EntityPropertyChange =
    { Address : Address
      PropertyInfo : PropertyInfo
      Value : obj }

let getEntityTypes (entity : Entity) =
    match entity.Subtype with
    | Gui gui ->
        let guiSemType =
            match gui.SubSubtype with
            | Button _ -> typeof<Button>
            | Label _ -> typeof<Label>
            | TextBox _ -> typeof<TextBox>
            | Toggle _ -> typeof<Toggle>
            | Feeler _ -> typeof<Feeler>
        [typeof<Entity>; typeof<Gui>; guiSemType]
    | Actor actor ->
        let actorSemType =
            match actor.SubSubtype with
            | Block _ -> typeof<Block>
            | Avatar _ -> typeof<Avatar>
            | TileMap _ -> typeof<TileMap>
        [typeof<Entity>; typeof<Actor>; actorSemType]

let getEntityPropertyValue (property : PropertyInfo) (entity : Entity) =
    if typeof<Entity>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
    then property.GetValue entity
    else
        match entity.Subtype with
        | Gui gui ->
            if typeof<Gui>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then property.GetValue gui
            else
                match gui.SubSubtype with
                | Button button -> property.GetValue button
                | Label label -> property.GetValue label
                | TextBox textBox -> property.GetValue textBox
                | Toggle toggle -> property.GetValue toggle
                | Feeler feeler -> property.GetValue feeler
        | Actor actor ->
            if typeof<Actor>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then property.GetValue actor
            else
                match actor.SubSubtype with
                | Block block -> property.GetValue block
                | Avatar avatar -> property.GetValue avatar
                | TileMap tileMap -> property.GetValue tileMap

let setEntityPropertyValue world (change : EntityPropertyChange) =
    let entityLens = World.entity change.Address
    let entity = get world entityLens
    let property = change.PropertyInfo
    let value = change.Value
    let entity_ =
        let entity_ = { entity with Id = entity.Id } // NOTE: this is just a hacky way to copy an entity in lieu of reflection
        if typeof<Entity>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
        then let _ = property.SetValue (entity_, value) in entity_
        else
            match entity.Subtype with
            | Gui gui ->
                let gui_ = { gui with Position = gui.Position } // NOTE: hacky copy
                if typeof<Gui>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (gui_, value) in { entity with Subtype = Gui gui_ }
                else
                    match gui.SubSubtype with
                    | Button button ->
                        let button_ = { button with IsDown = button.IsDown } // NOTE: hacky copy
                        property.SetValue (button_, value)
                        { entity with Subtype = Gui { gui with SubSubtype = Button button_ }}
                    | Label label ->
                        let label_ = { label with LabelSprite = label.LabelSprite } // NOTE: hacky copy
                        property.SetValue (label_, value)
                        { entity with Subtype = Gui { gui with SubSubtype = Label label_ }}
                    | TextBox textBox ->
                        let textBox_ = { textBox with BoxSprite = textBox.BoxSprite } // NOTE: hacky copy
                        property.SetValue (textBox_, value)
                        { entity with Subtype = Gui { gui with SubSubtype = TextBox textBox_ }}
                    | Toggle toggle ->
                        let toggle_ = { toggle with IsPressed = toggle.IsPressed } // NOTE: hacky copy
                        property.SetValue (toggle_, value)
                        { entity with Subtype = Gui { gui with SubSubtype = Toggle toggle_ }}
                    | Feeler feeler ->
                        let feeler_ = { feeler with IsTouched = feeler.IsTouched } // NOTE: hacky copy
                        property.SetValue (feeler_, value)
                        { entity with Subtype = Gui { gui with SubSubtype = Feeler feeler_ }}
            | Actor actor ->
                let actor_ = { actor with Position = actor.Position } // NOTE: hacky copy
                if typeof<Actor>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (actor_, value) in { entity with Subtype = Actor actor_ }
                else
                    match actor.SubSubtype with
                    | Block block ->
                        let block_ = { block with PhysicsId = block.PhysicsId } // NOTE: hacky copy
                        property.SetValue (block_, value)
                        { entity with Subtype = Actor { actor with SubSubtype = Block block_ }}
                    | Avatar avatar ->
                        let avatar_ = { avatar with PhysicsId = avatar.PhysicsId } // NOTE: hacky copy
                        property.SetValue (avatar_, value)
                        { entity with Subtype = Actor { actor with SubSubtype = Avatar avatar_ }}
                    | TileMap tileMap ->
                        let tileMap_ = { tileMap with PhysicsIds = tileMap.PhysicsIds } // NOTE: hacky copy
                        property.SetValue (tileMap_, value)
                        { entity with Subtype = Actor { actor with SubSubtype = TileMap tileMap_ }}
    set entity_ world entityLens

let setEntityPropertyValues changes world =
    Seq.fold setEntityPropertyValue world changes