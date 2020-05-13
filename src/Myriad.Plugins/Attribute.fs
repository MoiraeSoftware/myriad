namespace Myriad.Plugins

open System


[<RequireQualifiedAccess>]
module Generator =
    type FieldsAttribute() =
        inherit Attribute()

    type DuCasesAttribute() =
        inherit Attribute()