namespace Myriad.Plugins

open System
open Myriad.Core

[<RequireQualifiedAccess>]
module Generator =
    type FieldsAttribute(configGroup: string) =
        inherit Attribute()

    type DuCasesAttribute(configGroup: string) =
        inherit Attribute()

    /// Instructs to generate lenses for each property of the record
    type LensesAttribute(configGroup: string, wrapperName : string option) =
        inherit Attribute()
        let mutable _wrapperName = wrapperName
        member this.WrapperName = _wrapperName
        new (config: string, wrapperType: Type) = LensesAttribute(config, Some wrapperType.Name)
        new (config: string, wrapperName: string) = LensesAttribute(config, Some wrapperName)


    let getConfigFromAttribute<'a> (configGetter: string -> seq<string * obj>) typeDef =
        match Ast.getAttribute<'a> typeDef with
        | None -> Seq.empty
        | Some a ->
            match Ast.getAttributeConstants a with
            | [] -> Seq.empty
            | [name] -> configGetter name
            | [name;_] -> configGetter name
            | others -> failwithf "More than two constants are not yet supported: %A" others
