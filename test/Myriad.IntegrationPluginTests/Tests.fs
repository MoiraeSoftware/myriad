module Tests

open Expecto
open Example
open Example.Lens
open Input
open UnknownNamespace

let tests =
    testList "basic tests" [

        test "Test txt based module generator generated with config" {
            Expect.equal TestExample1.First.fourtyTwo 42 "generated value should be 42"
            Expect.equal TestExample1.Second.fourtyTwo 42 "generated value should be 42"
            Expect.equal TestExample1.Third.fourtyTwo 42 "generated value should be 42"
            Expect.equal TestExample1.Fourth.fourtyTwo 42 "generated value should be 42"
        }

        test "Test txt based module generator generated no config" {
            Expect.equal UnknownNamespace.First.fourtyTwo 42 "generated value should be 42"
            Expect.equal UnknownNamespace.Second.fourtyTwo 42 "generated value should be 42"
            Expect.equal UnknownNamespace.Third.fourtyTwo 42 "generated value should be 42"
            Expect.equal UnknownNamespace.Fourth.fourtyTwo 42 "generated value should be 42"
        }

        test "Test1 create Test" {
            let t = TestFields.Test1.create 1 "2" 3. (float32 4)
            Expect.equal t {Test1.one = 1; two = "2"; three = 3.; four = float32 4 } "generated records should be ok"
        }

        test "Test1 accessor Test" {
            let t = TestFields.Test1.create 1 "2" 3. (float32 4)
            let z = TestFields.Test1.one t
            Expect.equal z 1 "generated getters should be ok"
        }

        testList "Lenses" [
            testList "Records" [
                let t = TestFields.Test1.create 1 "2" 3. (float32 4)

                test "Getter" {
                    let getter = fst TestLens.Test1Lenses.one
                    Expect.equal 1 (getter t) "getter returns the value"
                }

                test "Setter" {
                    let setter = snd TestLens.Test1Lenses.one
                    let updated = setter t 2
                    Expect.equal 2 updated.one "setter updates the value"
                }

                test "Wrapped getter" {
                    let (Lens(getter, _)) = TestLens.RecordWithWrappedLensLenses.one
                    let src : RecordWithWrappedLens = { one = 1 }
                    let value = getter src
                    Expect.equal 1 value "getter returns the value"
                }

                test "Wrapped setter" {
                    let (Lens(_, setter)) = TestLens.RecordWithWrappedLensLenses.one
                    let src : RecordWithWrappedLens = { one = 1 }
                    let updated = setter src 2
                    Expect.equal { one = 2 } updated "setter updates the value"
                }

                test "Empty wrapper name" {
                    let (getter, _) = TestLens.RecordWithEmptyWrapperNameLenses.one_empty_wrapper_name
                    let src = { one_empty_wrapper_name = 1 }
                    Expect.equal 1 (getter src) "getter returns the value"
                }
            ]

            testList "Single-case DUs" [
                test "Unwrapped getter" {
                    let getter = fst TestLens.SingleCaseDULenses.Lens'
                    let t = Single 1

                    Expect.equal (getter t) 1 "getter returns the value"
                }
                test "Unwrapped setter" {
                    let setter = snd TestLens.SingleCaseDULenses.Lens'
                    let t = Single 1

                    let updated = setter t 2
                    let (Single actualValue) = updated
                    Expect.equal actualValue 2 "getter returns the value"
                }
                test "Wrapped getter" {
                    let (Lens (getter, _)) = TestLens.WrappedSingleCaseDULenses.Lens'
                    let t = SingleWrapped 1

                    Expect.equal (getter t) 1 "getter returns the value"
                }
                test "Wrapped setter" {
                    let (Lens (_, setter)) = TestLens.WrappedSingleCaseDULenses.Lens'
                    let t = SingleWrapped 1

                    let updated = setter t 2
                    let (SingleWrapped actualValue) = updated
                    Expect.equal actualValue 2 "getter returns the value"
                }
            ]

            test "Lens composition" {
                let houseNumberLens = TestLens.PersonLenses.Address << TestLens.AddressLenses.HouseNumber
                let person: Person = {
                    Name = "Sherlock"
                    Address = {
                        Street = "Baker st."
                        HouseNumber = 221
                    }
                }

                let houseNumber = Lens.get houseNumberLens person

                Expect.equal houseNumber 221 "Gets correct house number"

                let updatedPerson = person |> Lens.set houseNumberLens 1
                let updatedHouseNumber = Lens.get houseNumberLens updatedPerson

                Expect.equal updatedHouseNumber 1 "Gets updated value"
            }
            
            test "Aether get" {
                let person: AetherPerson = {
                    Name = "Sherlock"
                    Address = {
                        Street = "Baker st."
                        HouseNumber = 221
                    }
                }
                let value =  Aether.Optic.get AetherTestLens.AetherPersonLenses.Address person
                Expect.equal value person.Address "Gets the address lens via Aether"
            }
        ]
    ]