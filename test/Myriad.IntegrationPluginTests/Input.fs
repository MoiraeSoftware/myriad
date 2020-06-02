namespace Example

open Myriad.Plugins

type Lens<'r, 't> = Lens of (('r -> 't) * ('r -> 't -> 'r))

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
type Test1WithWrappedLens =
    { one: int }

[<Generator.Lenses("Lens")>]
type Test1WithEmptyWrapperName =
    { one_empty_wrapper_name: int }

[<Generator.Lenses(typedefof<Lens<_, _>>)>]
type Test1WithWrappedLensWithTypedefof =
    { one_typedefof: Option<int> }

[<Generator.Lenses(typeof<Lens<_, _>>)>]
type Test1WithWrappedLensWithTypeof =
    { one_typeof: Option<int> }

[<Generator.DuCases>]
type Currency =
    | CAD
    | PLN
    | EUR
    | USD
    | Custom of string
