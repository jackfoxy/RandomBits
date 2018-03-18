module RandomBits.Tests.Int32

open RandomBits
open Expecto

[<Tests>]
let int32Tests =
    testList "Int32Tests" [

        testCase "random int32 in range from first mock byte" <| fun () ->
            let x = new RandomBits("A800000000000000")
            let a = x.RndInt32 (-12, -2) 
            let b = -4
            Expect.equal a b "Expected equal"

        testCase "random int32 in range from first mock byte 2" <| fun () ->
            let x = new RandomBits("A800000000000000")
            let a = x.RndInt32 (-2, 6) 
            let b = 3
            Expect.equal a b "Expected equal"

        testCase "random u and int32 from first mock byte" <| fun () ->
            let x = new RandomBits("7000000000000000")
            Expect.equal (x.RndUint32()) 1879048192u "Expected equal"
            Expect.equal (x.RndInt32()) 1879048192 "Expected equal"
            Expect.equal x.Consume64Count 2L "Expected equal"

        testCase "random uint32 in range from first mock byte" <| fun () ->
            let x = new RandomBits("A800000000000000")
            let a = x.RndUint32 (0u, 6u) 
            let b = 5u
            Expect.equal a b "Expected equal"

        testCase "random u and int32 in range of max range" <| fun () ->
            let x = new RandomBits("A800000000000000")
            Expect.equal (x.RndUint32 (System.UInt32.MinValue, System.UInt32.MaxValue)) 2818572288u "Expected equal"
            Expect.equal (x.RndInt32 (System.Int32.MinValue, System.Int32.MaxValue)) -2147483648 "Expected equal"

        testCase "s Seq length of 8 consumes 4 64-bit cache block" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = List.ofSeq (x.RndInt32Seq 8) 
            let b = [2014707729; 571474227; 2014707729; 571474227; 2014707729; 571474227; 2014707729; 571474227]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 4L "Expected equal"

        testCase "s Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt32Seq 0) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range length of less than 1 fails2" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt32Seq (1, 2, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt32Seq (1, 2, 2)) 
            let b = [1; 1]
            Expect.equal a b "Expected equal"

        testCase "s Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt32Seq (1, 1, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range of max range" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq (x.RndInt32Seq (System.Int32.MinValue, System.Int32.MaxValue, 255))).Length 
            let b = 255
            Expect.equal a b "Expected equal"

        testCase "s Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt32Seq (0, 6, 2)) 
            let b = [5; 2]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "s Unique Seq of length = range succeed" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq ( x.RndInt32UniqueSeq (2, 12, 10))).Length 
            let b = 10
            Expect.equal a b "Expected equal"

        testCase "s Unique Seq of length gt range should fail" <| fun () ->
            let x = new RandomBits("7816001122100133")
            Expect.throwsT<System.ArgumentException>
                (fun () -> x.RndInt32UniqueSeq (2, 12, 11) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt32UniqueSeq (1, 2, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt32UniqueSeq (1, 1, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt32UniqueSeq (1, 2, 1)) 
            let b = [1]
            Expect.equal a b "Expected equal"

        testCase "s Unique Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt32UniqueSeq (0, 6, 2)) 
            let b = [5; 2]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "u Seq length of 9 consumes 5 64-bit cache blocks" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = List.ofSeq (x.RndUint32Seq 9) 
            let b = [2014707729u; 571474227u; 2014707729u; 571474227u; 2014707729u; 571474227u; 2014707729u; 571474227u; 2014707729u]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 5L "Expected equal"

        testCase "u Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint32Seq 0) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint32Seq (1u, 2u, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint32Seq (1u, 1u, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint32Seq (1u, 2u, 2)) 
            let b = [1u; 1u]
            Expect.equal a b "Expected equal"

        testCase "u Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint32Seq (0u, 6u, 2)) 
            let b = [5u; 2u]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "u Unique Seq of length = range succeed" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq ( x.RndUint32UniqueSeq (2u, 12u, 10))).Length 
            let b = 10
            Expect.equal a b "Expected equal"

        testCase "u Unique Seq of length gt range should fail" <| fun () ->
            let x = new RandomBits("7816001122100133")
            Expect.throwsT<System.ArgumentException>
                (fun () -> x.RndUint32UniqueSeq (2u, 12u, 11) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint32UniqueSeq (1u, 2u, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint32UniqueSeq (1u, 2u, 1)) 
            let b = [1u]
            Expect.equal a b "Expected equal"

        testCase "u Unique Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint32UniqueSeq (1u, 1u, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint32UniqueSeq (0u, 6u, 2)) 
            let b = [5u; 2u]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"
        ]