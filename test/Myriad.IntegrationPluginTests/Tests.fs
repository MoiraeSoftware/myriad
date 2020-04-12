module Tests

open System
open Xunit

[<Fact>]
let ``Test namespace generated`` () =
    Assert.Equal(42, Test.example1.fourtyTwo)

[<Fact>]
let ``Test2 namespace generated`` () =
    Assert.Equal(42, Test2.example1.fourtyTwo)