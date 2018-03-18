module RandomBits.Tests.Bool

open RandomBits
open Expecto

[<Tests>]
let boolTests =

    testList "BoolTests" [
        testCase "2 rndBool calls consume 2 64-bit cache blocks" <| fun () ->
            let x = new RandomBits("7fffffffffffffff")
            let a = x.RndBool() 
            let b = x.RndBool()
            Expect.equal x.Consume64Count 2L "Expected equal"

        testCase "first bit false" <| fun () ->
            let x = new RandomBits("7fffffffffffffff")
            Expect.equal (x.RndBool()) false "Expected equal"

        testCase "first bit true" <| fun () ->
            let x = new RandomBits("8000000000000000")
            Expect.equal (x.RndBool()) true "Expected equal"

        testCase "Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndBoolSeq 0) |> ignore) "Expected ArgumentException"

        testCase "Seq of 64 consumes 1 64-bit cache block" <| fun () ->
            let x = new RandomBits("aF00000000000000")
            let a = List.ofSeq (x.RndBoolSeq 64) 
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "Seq of 65 consumes 2 64-bit cache blocks" <| fun () ->
            let x = new RandomBits("aF00000000000000")
            let a = List.ofSeq (x.RndBoolSeq 65)
            Expect.equal x.Consume64Count 2L "Expected equal"

        testCase "Seq returns predicted values" <| fun () ->
            let x = new RandomBits("aF00000000000000")
            let a = List.ofSeq (x.RndBoolSeq 5) 
            let b = [true; false; true; false; true]
            Expect.equal a b "Expected equal"

        ]