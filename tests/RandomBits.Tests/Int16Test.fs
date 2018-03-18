module RandomBits.Tests.Int16

open RandomBits
open Expecto

[<Tests>]
let int16Tests =
    testList "Int16Tests" [

        testCase "random int16 in range from first mock byte" <| fun () ->
            let x = new RandomBits("A800000000000000")
            let a = x.RndInt16 (-12s, -2s) 
            let b = -4s
            Expect.equal a b "Expected equal"

        testCase "random int16 in range from first mock byte 2" <| fun () ->
            let x = new RandomBits("A800000000000000")
            let a = x.RndInt16 (-2s, 6s) 
            let b = 3s
            Expect.equal a b "Expected equal"

        testCase "random u and int16 from first mock byte" <| fun () ->
            let x = new RandomBits("7000000000000000")
            Expect.equal (x.RndUint16()) 28672us "Expected equal"
            Expect.equal (x.RndInt16()) 28672s "Expected equal"
            Expect.equal x.Consume64Count 2L "Expected equal"

        testCase "random uint16 in range from first mock byte" <| fun () ->
            let x = new RandomBits("A800000000000000")
            let a = x.RndUint16 (0us, 6us)
            let b = 5us
            Expect.equal a b "Expected equal"

        testCase "random u and int16 in range of max range" <| fun () ->
            let x = new RandomBits("A800000000000000")
            Expect.equal (x.RndUint16 (System.UInt16.MinValue, System.UInt16.MaxValue)) 43008us "Expected equal"
            Expect.equal (x.RndInt16 (System.Int16.MinValue, System.Int16.MaxValue)) -32768s "Expected equal"

        testCase "s Seq length of 8 consumes 2 64-bit cache block" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = List.ofSeq (x.RndInt16Seq 8) 
            let b = [30742s; 17s; 8720s; 307s; 30742s; 17s; 8720s; 307s]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 2L "Expected equal"

        testCase "s Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt16Seq 0) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt16Seq (1s, 2s, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt16Seq (1s, 2s, 2)) 
            let b = [1s; 1s]
            Expect.equal a b "Expected equal"

        testCase "s Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt16Seq (1s, 1s, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range of max range" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq (x.RndInt16Seq (System.Int16.MinValue, System.Int16.MaxValue, 255))).Length 
            let b = 255
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 64L "Expected equal"

        testCase "s Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt16Seq (0s, 6s, 2))
            let b = [5s; 2s]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "s Unique Seq of length = range succeed" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq ( x.RndInt16UniqueSeq (2s, 12s, 10))).Length 
            let b = 10
            Expect.equal a b "Expected equal"

        testCase "s Unique Seq of length gt range should fail" <| fun () ->
            let x = new RandomBits("7816001122100133")
            Expect.throwsT<System.ArgumentException>
                (fun () -> x.RndInt16UniqueSeq (2s, 12s, 11) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt16UniqueSeq (1s, 2s, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt16UniqueSeq (1s, 1s, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt16UniqueSeq (1s, 2s, 1)) 
            let b = [1s]
            Expect.equal a b "Expected equal"

        testCase "s Unique Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt16UniqueSeq (0s, 6s, 2))
            let b = [5s; 2s]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "u Seq length of 9 consumes 3 64-bit cache blocks" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = List.ofSeq (x.RndUint16Seq 9)
            let b = [30742us; 17us; 8720us; 307us; 30742us; 17us; 8720us; 307us; 30742us]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 3L "Expected equal"

        testCase "u Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint16Seq 0) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint16Seq (1us, 2us, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint16Seq (1us, 1us, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint16Seq (1us, 2us, 2))
            let b = [1us; 1us]
            Expect.equal a b "Expected equal"

        testCase "`u Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint16Seq (0us, 6us, 2)) 
            let b = [5us; 2us]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "u Unique Seq of length = range succeed" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq ( x.RndUint16UniqueSeq (2us, 12us, 10))).Length 
            let b = 10
            Expect.equal a b "Expected equal"

        testCase "u Unique Seq of length gt range should fail" <| fun () ->
            let x = new RandomBits("7816001122100133")
            Expect.throwsT<System.ArgumentException>
                (fun () -> x.RndUint16UniqueSeq (2us, 12us, 11) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint16UniqueSeq (1us, 2us, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint16UniqueSeq (1us, 2us, 1)) 
            let b = [1us]
            Expect.equal a b "Expected equal"

        testCase "u Unique Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint16UniqueSeq (1us, 1us, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint16UniqueSeq (0us, 6us, 2)) 
            let b = [5us; 2us]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"
        ]