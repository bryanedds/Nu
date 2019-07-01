// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu.Gaia
open OpenTK
open System
open System.ComponentModel
open System.Collections.Generic
open System.Windows.Forms
open Prime
open Nu
open Nu.Gaia
open Nu.Gaia.Design

// TODO: consider getting rid of the duplication of code from GaiaEntity.fs.

type [<TypeDescriptionProvider (typeof<LayerTypeDescriptorProvider>)>] LayerTypeDescriptorSource =
    { DescribedLayer : Layer
      Form : GaiaForm }

and LayerPropertyDescriptor (property, attributes) =
    inherit System.ComponentModel.PropertyDescriptor (
        (match property with LayerPropertyDescriptor xfd -> xfd.PropertyName | LayerPropertyInfo pi -> pi.Name),
        attributes)

    let propertyName = match property with LayerPropertyDescriptor pd -> pd.PropertyName | LayerPropertyInfo pi -> pi.Name
    let propertyType = match property with LayerPropertyDescriptor pd -> pd.PropertyType | LayerPropertyInfo pi -> pi.PropertyType
    let propertyCanWrite = match property with LayerPropertyDescriptor _ -> true | LayerPropertyInfo xfd -> xfd.CanWrite

    override this.Category =
        // HACK: all of this stuff is a hack until we can get user-defined attributes on simulant properties!
        // HACK: in order to put the Events as the last category, I start all the other categories with an unprinted
        // \r character as here - https://bytes.com/topic/c-sharp/answers/214456-q-ordering-sorting-category-text-propertygrid
        if propertyName.Length > 2 && propertyName.StartsWith "On" && Char.IsUpper propertyName.[2] then "Events"
        elif propertyName = "Name" then "\rAmbient Properties"
        elif propertyName = "Persistent" || propertyName = "Script" || propertyName = "ScriptOpt" || propertyName = "Depth" || propertyName = "Visible" then "\rScene Properties"
        elif propertyType = typeof<DesignerProperty> then "\rDesigner Properties"
        else "\rXtension Properties"

    override this.Description =
        // HACK: lets user know the property's expected type
        Reflection.getSimplifiedTypeNameHack propertyType

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
            let layerTds = source :?> LayerTypeDescriptorSource
            LayerPropertyValue.getValue property layerTds.DescribedLayer Globals.World

    override this.SetValue (source, value) =
        
        // grab the type descriptor and assign the value
        let layerTds = source :?> LayerTypeDescriptorSource
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
                MessageBox.Show
                    ("Changing the name of a layer after it has been created is not yet implemented.",
                     "Cannot change layer name in Gaia.",
                     MessageBoxButtons.OK) |>
                    ignore
                world

            // change the property dynamically
            | _ ->
                let layer = layerTds.DescribedLayer
                let world =
                    match propertyName with
                    | "OverlayNameOpt" ->
                        MessageBox.Show
                            ("Changing the overlay of a layer after it has been created is not yet implemented.",
                             "Cannot change layer overlay in Gaia.",
                             MessageBoxButtons.OK) |>
                            ignore
                        world
                    | _ -> LayerPropertyValue.setValue property value layer world
                Globals.World <- world // must be set for property grid
                layerTds.Form.layerPropertyGrid.Refresh ()
                world)

        // NOTE: in order to update the view immediately, we have to apply the changer twice,
        // once immediately and once in the update function
        Globals.World <- changer Globals.World
        Globals.WorldChangers.Add changer |> ignore

and LayerTypeDescriptor (sourceOpt : obj) =
    inherit CustomTypeDescriptor ()

    override this.GetProperties () =
        let contextOpt =
            match sourceOpt with
            | :? LayerTypeDescriptorSource as source -> Some (source.DescribedLayer, Globals.World)
            | _ -> None
        let makePropertyDescriptor = fun (epv, tcas) -> (LayerPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
        let propertyDescriptors = LayerPropertyValue.getPropertyDescriptors makePropertyDescriptor contextOpt
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    override this.GetProperties _ =
        this.GetProperties ()

and LayerTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, sourceOpt) = LayerTypeDescriptor sourceOpt :> ICustomTypeDescriptor
