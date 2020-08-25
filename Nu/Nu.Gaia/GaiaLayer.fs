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

// TODO: consider getting rid of the duplication of code from GaiaEntity.fs.

type [<TypeDescriptionProvider (typeof<LayerTypeDescriptorProvider>)>] LayerTypeDescriptorSource =
    { DescribedLayer : Layer
      Form : GaiaForm }

and LayerPropertyDescriptor (propertyDescriptor, attributes) =
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
        if propertyName.EndsWith "Script" then "Scripts"
        elif propertyName = "Name" then "\rAmbient Properties"
        elif propertyName = "Persistent" || propertyName = "Script" || propertyName = "ScriptOpt" || propertyName = "Depth" || propertyName = "Visible" then "\rScene Properties"
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
        Reflection.isPropertyNonPersistentByName propertyName

    override this.SetValue (source, value) =
        Globals.WorldChangers.Add $ fun world ->
        
            // grab the type descriptor and layer
            let layerTds = source :?> LayerTypeDescriptorSource
            let layer = layerTds.DescribedLayer

            // pull string quotes out of string
            let value =
                match value with
                | :? string as str -> str.Replace ("\"", "") :> obj
                | _ -> value

            // make property change undo-able
            Globals.pushPastWorld world
            let world = layer.Diverge world
            
            // change property
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
                let world =
                    match propertyName with
                    | "OverlayNameOpt" ->
                        MessageBox.Show
                            ("Changing the overlay of a layer after it has been created is not yet implemented.",
                             "Cannot change layer overlay in Gaia.",
                             MessageBoxButtons.OK) |>
                            ignore
                        world
                    | _ ->
                        let (_, world) = PropertyDescriptor.trySetValue true false propertyDescriptor value layer world
                        world
                Globals.World <- world // must be set for property grid
                layerTds.Form.layerPropertyGrid.Refresh ()
                world

    override this.GetValue source =
        match source with
        | null -> null // WHY THE FUCK IS THIS EVER null???
        | source ->
            let layerTds = source :?> LayerTypeDescriptorSource
            PropertyDescriptor.tryGetValue propertyDescriptor layerTds.DescribedLayer Globals.World |> Option.get

and LayerTypeDescriptor (sourceOpt : obj) =
    inherit CustomTypeDescriptor ()

    override this.GetProperties () =
        let contextOpt =
            match sourceOpt with
            | :? LayerTypeDescriptorSource as source -> Some (source.DescribedLayer :> Simulant, Globals.World)
            | _ -> None
        let makePropertyDescriptor = fun (epv, tcas) -> (LayerPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
        let propertyDescriptors = PropertyDescriptor.getPropertyDescriptors<LayerState> makePropertyDescriptor contextOpt
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    override this.GetProperties _ =
        this.GetProperties ()

and LayerTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, sourceOpt) = LayerTypeDescriptor sourceOpt :> ICustomTypeDescriptor
