// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.ComponentModel
open Prime
open Nu
open Nu.Gaia

type [<TypeDescriptionProvider (typeof<EntityTypeDescriptorProvider>)>] EntityTypeDescriptorSource =
    { DescribedEntity : Entity }

and EntityPropertyDescriptor (propertyDescriptor, attributes) =
    inherit System.ComponentModel.PropertyDescriptor (propertyDescriptor.PropertyName, attributes)

    let propertyName =
        propertyDescriptor.PropertyName

    let propertyType =
        propertyDescriptor.PropertyType

    let propertyCanWrite =
        true

    override this.Category =
        // HACK: in order to put Scripts and Physics as the last categories, I start all the other categories with an
        // unprinted \r character as here - https://bytes.com/topic/c-sharp/answers/214456-q-ordering-sorting-category-text-propertygrid
        let baseProperties = Reflection.getPropertyDefinitions typeof<EntityDispatcher>
        let rigidBodyProperties = Reflection.getPropertyDefinitions typeof<RigidBodyFacet>
        if propertyName.EndsWith "Script" || propertyName.EndsWith "ScriptOpt" then "Scripts"
        elif propertyName = "Name" || propertyName = "Surnames" || propertyName = "Model" || propertyName = "MountOpt" || propertyName = "OverlayNameOpt" || propertyName = "FacetNames" then "\rAmbient Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) baseProperties then "\rBuilt-In Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) rigidBodyProperties then "Physics Properties"
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
        propertyName <> "Degrees" && propertyName <> "DegreesLocal" && // HACK: we allow degrees specifically for the editor.
        (not propertyCanWrite || Reflection.isPropertyNonPersistentByName propertyName)

    override this.GetValue source =
        match source with
        | null -> null // WHY THE FUCK IS THIS EVER null???
        | source ->
            let entityTds = source :?> EntityTypeDescriptorSource
            match PropertyDescriptor.tryGetValue propertyDescriptor entityTds.DescribedEntity Globals.World with
            | Some value -> value
            | None -> null

    override this.SetValue (source, value) =

        // grab the type descriptor and entity
        let entityTds = source :?> EntityTypeDescriptorSource
        let entity = entityTds.DescribedEntity

        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value

        // make property change undo-able
        Globals.pushPastWorld ()

        // change property
        match propertyName with
            
        // change the surnames property
        | "Surnames" ->
            let surnames = value :?> string array
            if Array.forall (fun (name : string) -> name.IndexOfAny Symbol.IllegalNameCharsArray = -1) surnames then
                let target = Nu.Entity (entity.Group.GroupAddress <-- rtoa surnames)
                Globals.World <- World.renameEntityImmediate entity target Globals.World
            else
                //DUMMY
                //MessageBox.Show
                //    ("Invalid entity surnames '" + scstring surnames + "'.",
                //     "Invalid Surnames",
                //     MessageBoxButtons.OK) |>
                //    ignore
                ()

        // change the name property
        | Constants.Engine.NamePropertyName ->
            let name = value :?> string
            if name.IndexOfAny Symbol.IllegalNameCharsArray = -1 then
                let targetNames =
                    entity.Group.GroupAddress.Names |>
                    flip Array.append (Array.allButLast entity.Surnames) |>
                    Array.add name
                let target = Nu.Entity targetNames
                Globals.World <- World.renameEntityImmediate entity target Globals.World
            else
                //DUMMY
                //MessageBox.Show
                //    ("Invalid entity name '" + name + "'.",
                //     "Invalid Name",
                //     MessageBoxButtons.OK) |>
                //    ignore
                ()

        // change facet names
        | Constants.Engine.FacetNamesPropertyName ->
            let facetNames = value :?> string Set
            Globals.World <-
                match World.trySetEntityFacetNames facetNames entity Globals.World with
                | (Right (), world) -> world
                | (Left error, world) -> Log.trace error; world

        // change the property dynamically
        | _ ->
            Globals.World <-
                match propertyName with
                | Constants.Engine.OverlayNameOptPropertyName ->
                    match World.trySetEntityOverlayNameOpt (value :?> string option) entity Globals.World with
                    | (Right (), world) -> world
                    | (Left error, world) -> Log.trace error; world
                | _ ->
                    let struct (_, _, world) = PropertyDescriptor.trySetValue propertyDescriptor value entity Globals.World
                    world

and EntityTypeDescriptor (sourceOpt : obj) =
    inherit CustomTypeDescriptor ()

    override this.GetProperties () =
        let contextOpt =
            match sourceOpt with
            | :? EntityTypeDescriptorSource as source -> Some (source.DescribedEntity :> Simulant, Globals.World)
            | _ -> None
        let makePropertyDescriptor = fun (epv, tcas) -> (EntityPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
        let nameDescriptor = makePropertyDescriptor ({ PropertyName = Constants.Engine.NamePropertyName; PropertyType = typeof<string> }, [||])
        let propertyDescriptors = PropertyDescriptor.getPropertyDescriptors<EntityState> makePropertyDescriptor contextOpt
        let propertyDescriptors = nameDescriptor :: propertyDescriptors
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    override this.GetProperties _ =
        this.GetProperties ()

and EntityTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, sourceOpt) = EntityTypeDescriptor sourceOpt :> ICustomTypeDescriptor