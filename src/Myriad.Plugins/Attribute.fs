namespace Myriad.Plugins

open System


[<RequireQualifiedAccess>]
module Generator =
    type FieldsAttribute(configGroup: string) =
        inherit Attribute()

    type DuCasesAttribute(configGroup: string) =
        inherit Attribute()