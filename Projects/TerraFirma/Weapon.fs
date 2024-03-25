namespace TerraFirma
open System
open Prime
open Nu

[<AutoOpen>]
module WeaponDispatcher =

    type Weapon =
        { StaticModel : StaticModel AssetTag }

    type WeaponDispatcher () =
        inherit Entity3dDispatcher<Weapon, Message, Command> (true, { StaticModel = Assets.Default.StaticModel })

        static member Facets =
            [typeof<StaticModelFacet>
             typeof<RigidBodyFacet>]

        override this.Initialize (template, _) =
            [Entity.StaticModel := template.StaticModel
             Entity.BodyType == Static
             Entity.BodyShape == SphereShape { Radius = 0.5f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.25f 0.0f)); PropertiesOpt = None }
             Entity.Sensor == true]