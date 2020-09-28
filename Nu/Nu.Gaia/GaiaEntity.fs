// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
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

and EntityPropertyDescriptor (propertyDescriptor, attributes) =
    inherit System.ComponentModel.PropertyDescriptor (propertyDescriptor.PropertyName, attributes)

    let propertyName =
        propertyDescriptor.PropertyName

    let propertyType =
        propertyDescriptor.PropertyType

    let propertyCanWrite =
        true

    override this.Category =
        // HACK: in order to put Scripts as the last category, I start all the other categories with an unprinted
        // \r character as here - https://bytes.com/topic/c-sharp/answers/214456-q-ordering-sorting-category-text-propertygrid
        let baseProperties = Reflection.getPropertyDefinitions typeof<EntityDispatcher>
        let nodeProperties = Reflection.getPropertyDefinitions typeof<NodeFacet>
        let rigidBodyProperties = Reflection.getPropertyDefinitions typeof<RigidBodyFacet>
        if propertyName.EndsWith "Script" then "Scripts"
        elif propertyName = "Name" || propertyName = "OverlayNameOpt" || propertyName = "FacetNames" || propertyName = "PublishChanges" then "\rAmbient Properties"
        elif propertyName.EndsWith "Model" then "\rScene Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) baseProperties then "\rScene Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) nodeProperties then "\rScene Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) rigidBodyProperties then "\rPhysics Properties"
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
        Reflection.isPropertyNonPersistentByName propertyName

    override this.GetValue source =
        match source with
        | null -> null // WHY THE FUCK IS THIS EVER null???
        | source ->
            let entityTds = source :?> EntityTypeDescriptorSource
            match PropertyDescriptor.tryGetValue propertyDescriptor entityTds.DescribedEntity Globals.World with
            | Some value -> value
            | None -> null

    override this.SetValue (source, value) =
        Globals.WorldChangers.Add $ fun world ->

            // grab the type descriptor and entity
            let entityTds = source :?> EntityTypeDescriptorSource
            let entity = entityTds.DescribedEntity

            // pull string quotes out of string
            let value =
                match value with
                | :? string as str -> str.Replace ("\"", "") :> obj
                | _ -> value

            // make property change undo-able
            Globals.pushPastWorld world
            let world = entity.Diverge world

            // change property
            match propertyName with
            
            // change the name property
            | "Name" ->
                let name = value :?> string
                if name.IndexOfAny Symbol.IllegalNameCharsArray = -1 then
                    let (entity, world) = World.reassignEntityImmediate entity (Some name) entity.Parent world
                    Globals.World <- world // must be set for property grid
                    Globals.SelectEntity entity Globals.Form world
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
                let world =
                    match World.trySetEntityFacetNames facetNames entity world with
                    | (Right (), world) -> world
                    | (Left error, world) -> Log.trace error; world
                Globals.World <- world // must be set for property grid
                entityTds.Form.entityPropertyGrid.Refresh ()
                world

            // change the property dynamically
            | _ ->
                let world =
                    match propertyName with
                    | "OverlayNameOpt" ->
                        match World.trySetEntityOverlayNameOpt (value :?> string option) entity world with
                        | (Right (), world) -> world
                        | (Left error, world) -> Log.trace error; world
                    | _ ->
                        let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                        let nonPersistent = Reflection.isPropertyNonPersistentByName propertyName
                        PropertyDescriptor.trySetValue alwaysPublish nonPersistent propertyDescriptor value entity world |> snd
                Globals.World <- world // must be set for property grid
                entityTds.Form.entityPropertyGrid.Refresh ()
                world

and EntityTypeDescriptor (sourceOpt : obj) =
    inherit CustomTypeDescriptor ()

    override this.GetProperties () =
        let contextOpt =
            match sourceOpt with
            | :? EntityTypeDescriptorSource as source -> Some (source.DescribedEntity :> Simulant, Globals.World)
            | _ -> None
        let makePropertyDescriptor = fun (epv, tcas) -> (EntityPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
        let propertyDescriptors = PropertyDescriptor.getPropertyDescriptors<EntityState> makePropertyDescriptor contextOpt
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    override this.GetProperties _ =
        this.GetProperties ()

and EntityTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, sourceOpt) = EntityTypeDescriptor sourceOpt :> ICustomTypeDescriptor
