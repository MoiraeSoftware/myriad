//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace rec Test2

module Test1 =
    open Example

    let one (x : Test1) = x.one
    let two (x : Test1) = x.two
    let three (x : Test1) = x.three
    let four (x : Test1) = x.four

    let create (one : int) (two : string) (three : float) (four : float32) : Test1 =
        { one = one
          two = two
          three = three
          four = four }

module Test2 =
    open Example

    let one (x : Test2) = x.one
    let two (x : Test2) = x.two

    let create (one : Test1) (two : string) : Test2 =
        { one = one
          two = two }