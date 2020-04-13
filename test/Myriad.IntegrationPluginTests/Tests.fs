module Tests

open System
open Xunit
open Example

[<Fact>]
let ``Test namespace generated`` () =
    Assert.Equal(42, Test.example1.fourtyTwo)

[<Fact>]
let ``Test2 namespace generated`` () =
    Assert.Equal(42, Test2.example1.fourtyTwo)

[<Fact>]
let ``Test1 create Test`` () =
    let t = Test.Test1.create 1 "2" 3. (float32 4)
    Assert.Equal({Test1.one = 1; two = "2"; three = 3.; four = float32 4 }, t)

[<Fact>]
let ``Test2 create Test`` () =
    let t = Test2.Test1.create 1 "2" 3. (float32 4)
    Assert.Equal({Test1.one = 1; two = "2"; three = 3.; four = float32 4 }, t)

[<Fact>]
let ``Test1 accessor Test`` () =
    let t = Test.Test1.create 1 "2" 3. (float32 4)
    let z = Test.Test1.one t
    Assert.Equal(1, z)

[<Fact>]
let ``Test2 accessor Test`` () =
    let t = Test2.Test1.create 1 "2" 3. (float32 4)
    let z = Test2.Test1.one t
    Assert.Equal(1, z)