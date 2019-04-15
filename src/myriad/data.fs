open System

type WithFields() =
    inherit Attribute()

[<WithFields>]
type test =
    { field1: int
      field2: string }

[<WithFields>]
type test2 =
  { field21: int
    field22: string }