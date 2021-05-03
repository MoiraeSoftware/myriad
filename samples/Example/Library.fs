namespace Example

[<Myriad.Plugins.Generator.Fields("fields")>]
type Test1 = { one: int; two: string; three: float; fou: float32 }
type Test2 = { one: Test1; two: string }
