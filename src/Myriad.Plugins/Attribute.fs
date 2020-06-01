namespace Myriad.Plugins

open System


[<RequireQualifiedAccess>]
module Generator =
    type FieldsAttribute() =
        inherit Attribute()

    type DuCasesAttribute() =
        inherit Attribute()

    /// Instructs to generate lenses for each property of the record
    type LensesAttribute() =
        inherit Attribute()