namespace Input

open Myriad.Plugins

[<Generator.Lenses("lens")>]
[<Generator.Fields "fields">]
type Test1 = { one: int; two: string; three: float; four: float32 }
type Test2 = { one: Test1; two: string }

[<Generator.Lenses("lens", "Example.Lens")>]
type RecordWithWrappedLens =
    { one: int } 

[<Generator.Lenses ("lens")>] 
type RecordWithEmptyWrapperName =
    { one_empty_wrapper_name: int } 

[<Generator.Lenses("lens", typedefof<Example.Lens<_, _>>)>]
type RecordWithWrappedLensViaTypedefof =
    { one_typedefof: Option<int> }

[<Generator.Lenses("lens", typeof<Example.Lens<_, _>>)>]
type RecordWithWrappedLensViaTypeof =
    { one_typeof: Option<int> }

[<Generator.Lenses("lens")>]
type SingleCaseDU = Single of int

[<Generator.Lenses("lens", typeof<Example.Lens<_, _>>)>]
type WrappedSingleCaseDU = SingleWrapped of int

[<RequireQualifiedAccess>]
[<Generator.Lenses("lens")>]
type FullyQualifiedDU = FullyQualified of string

module ModuleWithDUs =
    [<Generator.Lenses("lens")>]
    type Module_SingleCaseDU = Single of int

    [<Generator.Lenses("lens", "Example.Lens")>]
    type Module_WrappedSingleCaseDU = SingleWrapped of int

    [<Generator.Lenses("lens")>]
    [<RequireQualifiedAccess>]
    type Module_FullyQualifiedDU = FullyQualifiedCase of int

[<Generator.Lenses("lens", "Example.Lens")>]
type Address = {
    Street : string
    HouseNumber : int
}

[<Generator.Lenses("lens", "Example.Lens")>]
type Person = {
    Name : string
    Address : Address
}

[<Generator.DuCases "dus">]
type Currency =
    | CAD
    | PLN
    | EUR
    | USD
    | Custom of string