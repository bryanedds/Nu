// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Gaia
open OpenTK
open Prime
open System
open System.ComponentModel
open System.Collections.Generic
open System.Windows.Forms
open Prime
open Nu
open Nu.Gaia
open Nu.Gaia.Design

type WorldChanger = World -> World

type WorldChangers = WorldChanger List

type DragEntityState =
    | DragEntityPosition of Vector2 * Vector2 * Entity
    | DragEntityRotation of Vector2 * Vector2 * Entity
    | DragEntityNone

type DragCameraState =
    | DragCameraPosition of Vector2 * Vector2
    | DragCameraNone

type EditorState =
    { TargetDir : string
      RightClickPosition : Vector2
      DragEntityState : DragEntityState
      DragCameraState : DragCameraState
      PastWorlds : World list
      FutureWorlds : World list
      SelectedLayer : Layer }

type [<TypeDescriptionProvider (typeof<EntityTypeDescriptorProvider>)>] EntityTypeDescriptorSource =
    { DescribedEntity : Entity
      Form : GaiaForm
      WorldChangers : WorldChangers
      RefWorld : World ref }

and EntityPropertyDescriptor (property, attributes) =
    inherit System.ComponentModel.PropertyDescriptor (
        (match property with EntityXPropertyDescriptor xfd -> xfd.PropertyName | EntityPropertyInfo pi -> pi.Name),
        attributes)

    let propertyName = match property with EntityXPropertyDescriptor xfd -> xfd.PropertyName | EntityPropertyInfo pi -> pi.Name
    let propertyType = match property with EntityXPropertyDescriptor xfd -> xfd.PropertyType | EntityPropertyInfo pi -> pi.PropertyType
    let propertyCanWrite = match property with EntityXPropertyDescriptor _ -> true | EntityPropertyInfo xfd -> xfd.CanWrite

    let pushPastWorld pastWorld world =
        World.updateUserState
            (fun editorState -> { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = [] })
            world

    override this.ComponentType = propertyType.DeclaringType
    override this.PropertyType = propertyType
    override this.CanResetValue _ = false
    override this.ResetValue _ = ()
    override this.ShouldSerializeValue _ = true

    override this.IsReadOnly =
        not propertyCanWrite ||
        not ^ Reflection.isPropertyPersistentByName propertyName

    override this.GetValue source =
        match source with
        | null -> null // WHY THE FUCK IS THIS EVER null???
        | source ->
            let entityTds = source :?> EntityTypeDescriptorSource
            EntityPropertyValue.getValue property entityTds.DescribedEntity !entityTds.RefWorld

    override this.SetValue (source, value) =
        
        // grab the type descriptor and assign the value
        let entityTds = source :?> EntityTypeDescriptorSource
        let changer = (fun world ->

            // pull string quotes out of string
            let value =
                match value with
                | :? string as str -> str.Replace ("\"", "") :> obj
                | _ -> value

            // TODO: comment
            let world = pushPastWorld world world
            match propertyName with
            | "Name" ->
                let name = value :?> string
                if name.IndexOfAny Symbol.IllegalNameCharsArray = -1 then
                    let entity = entityTds.DescribedEntity
                    let world = World.reassignEntity entity (Some name) (etol entity) world
                    entityTds.RefWorld := world // must be set for property grid
                    world
                else
                    MessageBox.Show
                        ("Invalid name '" + name + "'; must have no whitespace and none of the following characters: '" + Symbol.IllegalNameChars + "'.",
                         "Invalid Name",
                         MessageBoxButtons.OK) |>
                        ignore
                    world

            // TODO: comment
            | "FacetNames" ->
                let facetNames = value :?> string Set
                let entity = entityTds.DescribedEntity
                let world =
                    match World.trySetEntityFacetNames facetNames entity world with
                    | Right world -> world
                    | Left error -> Log.trace error; world
                entityTds.RefWorld := world // must be set for property grid
                entityTds.Form.propertyGrid.Refresh ()
                world

            // TODO: comment
            | _ ->
                let entity = entityTds.DescribedEntity
                let world =
                    match propertyName with
                    | "OverlayNameOpt" ->
                        match World.trySetEntityOverlayNameOpt (value :?> string option) entity world with
                        | Right world -> world
                        | Left error -> Log.trace error; world
                    | _ -> EntityPropertyValue.setValue property value entity world
                let world = World.propagateEntityPhysics entityTds.DescribedEntity world
                entityTds.RefWorld := world // must be set for property grid
                entityTds.Form.propertyGrid.Refresh ()
                world)

        // NOTE: in order to update the view immediately, we have to apply the changer twice,
        // once immediately and once in the update function
        entityTds.RefWorld := changer !entityTds.RefWorld
        entityTds.WorldChangers.Add changer |> ignore

and EntityTypeDescriptor (sourceOpt : obj) =
    inherit CustomTypeDescriptor ()

    override this.GetProperties () =
        let contextOpt =
            match sourceOpt with
            | :? EntityTypeDescriptorSource as source -> Some (source.DescribedEntity, !source.RefWorld)
            | _ -> None
        let makePropertyDescriptor = fun (epv, tcas) -> (EntityPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
        let propertyDescriptors = EntityPropertyValue.getPropertyDescriptors makePropertyDescriptor contextOpt
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    override this.GetProperties _ =
        this.GetProperties ()

and EntityTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, sourceOpt) = EntityTypeDescriptor sourceOpt :> ICustomTypeDescriptor