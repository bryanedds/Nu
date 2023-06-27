// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.ComponentModel
open Prime
open Nu
open Nu.Gaia

// TODO: consider getting rid of the duplication of code from GaiaEntity.fs.

type [<TypeDescriptionProvider (typeof<ScreenTypeDescriptorProvider>)>] ScreenTypeDescriptorSource =
    { DescribedScreen : Screen }

and ScreenPropertyDescriptor (propertyDescriptor, attributes) =
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
        if propertyName.EndsWith "Script" || propertyName.EndsWith "ScriptOpt" then "Scripts"
        elif propertyName = "Name" ||  propertyName.EndsWith "Model" then "\rAmbient Properties"
        elif propertyName = "Persistent" || propertyName = "Incoming" || propertyName = "Outgoing" || propertyName = "SlideOpt" then "\rBuilt-In Properties"
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

        // grab the type descriptor and screen
        let screenTds = source :?> ScreenTypeDescriptorSource
        let screen = screenTds.DescribedScreen

        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value

        // make property change undo-able
        Globals.pushPastWorld ()
            
        // change the name property
        match propertyName with
        | Constants.Engine.NamePropertyName ->
            //DUMMY
            //MessageBox.Show
            //    ("Changing the name of a screen is not yet implemented.",
            //     "Cannot change screen name in Gaia.",
            //     MessageBoxButtons.OK) |>
            //    ignore
            ()

        // change the property dynamically
        | _ ->
            Globals.World <-
                let struct (_, _, world) = PropertyDescriptor.trySetValue propertyDescriptor value screen Globals.World
                world

    override this.GetValue source =
        match source with
        | null -> null // WHY THE FUCK IS THIS EVER null???
        | source ->
            let screenTds = source :?> ScreenTypeDescriptorSource
            PropertyDescriptor.tryGetValue propertyDescriptor screenTds.DescribedScreen Globals.World |> Option.get

and ScreenTypeDescriptor (sourceOpt : obj) =
    inherit CustomTypeDescriptor ()

    override this.GetProperties () =
        let contextOpt =
            match sourceOpt with
            | :? ScreenTypeDescriptorSource as source -> Some (source.DescribedScreen :> Simulant, Globals.World)
            | _ -> None
        let makePropertyDescriptor = fun (epv, tcas) -> (ScreenPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
        let propertyDescriptors = PropertyDescriptor.getPropertyDescriptors<ScreenState> makePropertyDescriptor contextOpt
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    override this.GetProperties _ =
        this.GetProperties ()

and ScreenTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, sourceOpt) = ScreenTypeDescriptor sourceOpt :> ICustomTypeDescriptor