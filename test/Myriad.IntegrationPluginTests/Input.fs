namespace Example

open Myriad.Plugins

[<Generator.Fields>]
[<Generator.Lenses>]
type Test1 =
    { one: int
      two: string
      three: float
      four: float32 }

type Test2 =
    { one: Test1
      two: string }

[<Generator.Lenses("Lens")>]
type RecordWithWrappedLens =
    { one: int }

[<Generator.Lenses("")>]
type RecordWithEmptyWrapperName =
    { one_empty_wrapper_name: int }

[<Generator.Lenses(typedefof<Lens<_, _>>)>]
type RecordWithWrappedLensViaTypedefof =
    { one_typedefof: Option<int> }

[<Generator.Lenses(typeof<Lens<_, _>>)>]
type RecordWithWrappedLensViaTypeof =
    { one_typeof: Option<int> }

[<Generator.Lenses>]
type SingleCaseDU = Single of int

[<Generator.Lenses(typeof<Lens<_, _>>)>]
type WrappedSingleCaseDU = SingleWrapped of int

[<RequireQualifiedAccess>]
[<Generator.Lenses>]
type FullyQualifiedDU = FullyQualified of string

module ModuleWithDUs =
    [<Generator.Lenses>]
    type Module_SingleCaseDU = Single of int

    [<Generator.Lenses("Example.Lens")>]
    type Module_WrappedSingleCaseDU = SingleWrapped of int

    [<Generator.Lenses>]
    [<RequireQualifiedAccess>]
    type Module_FullyQualifiedDU = FullyQualifiedCase of int

[<Generator.Lenses("Lens")>]
type Address = {
    Street : string
    HouseNumber : int
}

[<Generator.Lenses("Lens")>]
type Person = {
    Name : string
    Address : Address
}

[<Generator.DuCases>]
type Currency =
    | CAD
    | PLN
    | EUR
    | USD
    | Custom of string
