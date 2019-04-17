namespace Example

module MyRecords =
    type Test1 = { One: int; Two: string; Three: float; Four: float32 }
    type Test2 = { One: Test1; Two: string }
