namespace Prime
open System
open System.Reflection

[<AutoOpen>]
module TypeExtension =

    /// Type type extension.
    type Type with

        static member GetPropertyByPreference (preference, properties) =
            let optPreferred = Seq.tryFind preference properties
            if Seq.isEmpty properties then None
            else
                match optPreferred with
                | None -> Some <| Seq.head properties
                | Some _ -> optPreferred

        member this.GetPropertyWritable (propertyName, bindingFlags) =
            Seq.tryFind
                (fun (property : PropertyInfo) -> property.Name = propertyName && property.CanWrite)
                (this.GetProperties bindingFlags)

        member this.GetProperties (propertyName, bindingFlags) =
            Seq.filter
                (fun (property : PropertyInfo) -> property.Name = propertyName)
                (this.GetProperties bindingFlags)

        member this.GetPropertiesWritable bindingFlags =
            Seq.filter
                (fun (property : PropertyInfo) -> property.CanWrite)
                (this.GetProperties bindingFlags)

        member this.GetPropertiesWritable (propertyName, bindingFlags) =
            Seq.filter
                (fun (property : PropertyInfo) -> property.Name = propertyName && property.CanWrite)
                (this.GetProperties bindingFlags)

        member this.GetPropertyByPreference (preference, propertyName, bindingFlags) =
            let properties = this.GetProperties (propertyName, bindingFlags)
            Type.GetPropertyByPreference (preference, properties)

        member this.GetPropertyPreferWritable (propertyName, bindingFlags) =
            this.GetPropertyByPreference ((fun (property : PropertyInfo) -> property.CanWrite), propertyName, bindingFlags)

        member this.GetPropertiesByPreference (preference, bindingFlags) =
            let propertiesGrouped =
                Seq.groupBy
                    (fun (property : PropertyInfo) -> property.Name)
                    (this.GetProperties bindingFlags)
            let optProperties =
                Seq.map
                    (fun (_, properties) -> Type.GetPropertyByPreference (preference, properties))
                    propertiesGrouped
            Seq.definitize optProperties

        member this.GetPropertiesPreferWritable bindingFlags =
            this.GetPropertiesByPreference ((fun (property : PropertyInfo) -> property.CanWrite), bindingFlags)