namespace Myriad.Plugins

open System

[<RequireQualifiedAccess>]
module Generator =
    type FieldsAttribute() =
        inherit Attribute()

    type DuCasesAttribute() =
        inherit Attribute()

    /// Instructs to generate lenses for each property of the record
    type LensesAttribute(wrapperName : string) =
        inherit Attribute()
        let mutable _wrapperName = wrapperName
        member this.WrapperName = _wrapperName
        new () = LensesAttribute(null : string)
        new (``type``: Type) = LensesAttribute(``type``.Name)

    type DUKindAttribute() =
        inherit Attribute()