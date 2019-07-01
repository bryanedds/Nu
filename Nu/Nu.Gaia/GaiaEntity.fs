// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu.Gaia
open OpenTK
open System
open System.ComponentModel
open System.Windows.Forms
open Prime
open Nu
open Nu.Gaia
open Nu.Gaia.Design

type [<TypeDescriptionProvider (typeof<EntityTypeDescriptorProvider>)>] EntityTypeDescriptorSource =
    { DescribedEntity : Entity
      Form : GaiaForm }

and EntityPropertyDescriptor (property, attributes) =
    inherit System.ComponentModel.PropertyDescriptor (
        (match property with EntityPropertyDescriptor xfd -> xfd.PropertyName | EntityPropertyInfo pi -> pi.Name),
        attributes)

    let propertyName =
        match property with
        | EntityPropertyDescriptor pd -> pd.PropertyName
        | EntityPropertyInfo pi -> pi.Name

    let propertyType =
        match property with
        | EntityPropertyDescriptor pd -> pd.PropertyType
        | EntityPropertyInfo pi -> pi.PropertyType

    let propertyCanWrite =
        match property with
        | EntityPropertyDescriptor _ -> true
        | EntityPropertyInfo xfd -> xfd.CanWrite

    override this.Category =
        // HACK: all of this stuff is a hack until we can get user-defined attributes on simulant properties!
        // HACK: in order to put the Events as the last category, I start all the other categories with an unprinted
        // \r character as here - https://bytes.com/topic/c-sharp/answers/214456-q-ordering-sorting-category-text-propertygrid
        let baseProperties = Reflection.getPropertyDefinitions typeof<EntityDispatcher>
        let nodeProperties = Reflection.getPropertyDefinitions typeof<NodeFacet>
        let rigidBodyProperties = Reflection.getPropertyDefinitions typeof<RigidBodyFacet>
        if propertyName.Length > 2 && propertyName.StartsWith "On" && Char.IsUpper propertyName.[2] then "Events"
        elif propertyName = "Name" || propertyName = "OverlayNameOpt" || propertyName = "FacetNames" || propertyName = "PublishChanges" then "\rAmbient Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) baseProperties then "\rScene Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) nodeProperties then "\rScene Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) rigidBodyProperties then "\rPhysics Properties"
        elif propertyType = typeof<DesignerProperty> then "\rDesigner Properties"
        else "\rXtension Properties"

    override this.Description =
        // merely lets user know the property's expected type
        propertyType.GetGenericName ()

    override this.ComponentType = propertyType.DeclaringType
    override this.PropertyType = propertyType
    override this.CanResetValue _ = false
    override this.ResetValue _ = ()
    override this.ShouldSerializeValue _ = true

    override this.IsReadOnly =
        not propertyCanWrite ||
        not (Reflection.isPropertyPersistentByName propertyName)

    override this.GetValue source =
        match source with
        | null -> null // WHY THE FUCK IS THIS EVER null???
        | source ->
            let entityTds = source :?> EntityTypeDescriptorSource
            match EntityPropertyValue.tryGetValue property entityTds.DescribedEntity Globals.World with
            | Some value -> value
            | None -> null

    override this.SetValue (source, value) =
        
        // grab the type descriptor and assign the value
        let entityTds = source :?> EntityTypeDescriptorSource
        let changer = (fun world ->

            // pull string quotes out of string
            let value =
                match value with
                | :? string as str -> str.Replace ("\"", "") :> obj
                | _ -> value

            // make property change undo-able
            Globals.pushPastWorld world
            match propertyName with
            
            // change the name property
            | "Name" ->
                let name = value :?> string
                if name.IndexOfAny Symbol.IllegalNameCharsArray = -1 then
                    let entity = entityTds.DescribedEntity
                    let world = World.reassignEntity entity (Some name) (etol entity) world
                    Globals.World <- world // must be set for property grid
                    world
                else
                    MessageBox.Show
                        ("Invalid name '" + name + "'; must have no whitespace and none of the following characters: '" + (String.escape Symbol.IllegalNameChars) + "'.",
                         "Invalid Name",
                         MessageBoxButtons.OK) |>
                        ignore
                    world

            // change facet names
            | "FacetNames" ->
                let facetNames = value :?> string Set
                let entity = entityTds.DescribedEntity
                let world =
                    match World.trySetEntityFacetNames facetNames entity world with
                    | (Right (), world) -> world
                    | (Left error, world) -> Log.trace error; world
                Globals.World <- world // must be set for property grid
                entityTds.Form.entityPropertyGrid.Refresh ()
                world

            // change the property dynamically
            | _ ->
                let entity = entityTds.DescribedEntity
                let world =
                    match propertyName with
                    | "OverlayNameOpt" ->
                        match World.trySetEntityOverlayNameOpt (value :?> string option) entity world with
                        | (Right (), world) -> world
                        | (Left error, world) -> Log.trace error; world
                    | _ ->
                        let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                        let nonPersistent = not (Reflection.isPropertyPersistentByName propertyName)
                        EntityPropertyValue.trySetValue alwaysPublish nonPersistent property value entity world |> snd
                let world = entityTds.DescribedEntity.PropagatePhysics world
                Globals.World <- world // must be set for property grid
                entityTds.Form.entityPropertyGrid.Refresh ()
                world)

        // NOTE: in order to update the view immediately, we have to apply the changer twice,
        // once immediately and once in the update function
        Globals.World <- changer Globals.World
        Globals.WorldChangers.Add changer |> ignore

and EntityTypeDescriptor (sourceOpt : obj) =
    inherit CustomTypeDescriptor ()

    override this.GetProperties () =
        let contextOpt =
            match sourceOpt with
            | :? EntityTypeDescriptorSource as source -> Some (source.DescribedEntity, Globals.World)
            | _ -> None
        let makePropertyDescriptor = fun (epv, tcas) -> (EntityPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
        let propertyDescriptors = EntityPropertyValue.getPropertyDescriptors makePropertyDescriptor contextOpt
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    override this.GetProperties _ =
        this.GetProperties ()

and EntityTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, sourceOpt) = EntityTypeDescriptor sourceOpt :> ICustomTypeDescriptor
