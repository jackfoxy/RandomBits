module RandomBits.Tests.Byte

open RandomBits
open Expecto

[<Tests>]
let byteTests =
    testList "ByteTests" [

        testCase "random sbyte in range from first mock byte" <| fun () ->
            let x = new RandomBits("A800000000000000")
            Expect.equal (x.RndSByte (-12y, -2y)) -4y "Expected equal"

        testCase "random sbyte in range from first mock byte 2" <| fun () ->
            let x = new RandomBits("A800000000000000")
            Expect.equal (x.RndSByte (-2y, 6y)) 3y "Expected equal"

        testCase "random u and sbyte from first mock byte" <| fun () ->
            let x = new RandomBits("7800000000000000")
            Expect.equal (x.RndByte()) 120uy "Expected equal"
            Expect.equal (x.RndSByte()) 120y "Expected equal"
            Expect.equal x.Consume64Count 2L "Expected equal"

        testCase "random ubyte in range from first mock byte" <| fun () ->
            let x = new RandomBits("A800000000000000")
            Expect.equal (x.RndByte (0uy, 6uy)) 5uy "Expected equal"

        testCase "random u and sbyte in range of max range" <| fun () ->
            let x = new RandomBits("A800000000000000")
            Expect.equal 
                (x.RndByte (System.Byte.MinValue, System.Byte.MaxValue))
                168uy "Expected equal"
            Expect.equal 
                (x.RndSByte (System.SByte.MinValue, System.SByte.MaxValue))
                -128y "Expected equal"

        testCase "s Seq length of 8 consumes 1 64-bit cache block" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = List.ofSeq (x.RndSByteSeq 8)
            let b = [120y; 22y; 0y; 17y; 34y; 16y; 1y; 51y]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"
                
        testCase "s Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndSByteSeq 0) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndSByteSeq (1y, 2y, 0)) |> ignore) 
                "Expected System.ArgumentException"

        testCase "s Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("7fffffffffffffff")
            let a = List.ofSeq <| x.RndSByteSeq (1y, 2y, 2)
            let b = [1y; 1y]
            Expect.equal a b "Expected equal"

        testCase "s Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndSByteSeq (1y, 1y, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range of max range" <| fun () ->
            let x = new RandomBits("7816001122100133")
            Expect.equal 
                (List.ofSeq (x.RndSByteSeq (System.SByte.MinValue, System.SByte.MaxValue, 255))).Length
                255 "Expected equal"

        testCase "s Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = x.RndSByteSeq (0y, 6y, 2) |> List.ofSeq
            let b = [5y; 2y]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "`s Seq returns predicted values" <| fun () ->
            let x = new RandomBits("7816000000000000")
            let a = List.ofSeq (x.RndByteSeq 2)
            let b = [120uy; 22uy]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "s Unique Seq of length = range succeed" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq ( x.RndSByteUniqueSeq (2y, 12y, 10))).Length 
            let b = 10
            Expect.equal a b "Expected equal"

        testCase "s Unique Seq of length gt range should fail" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> x.RndSByteUniqueSeq (2y, 12y, 11) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndSByteUniqueSeq (1y, 2y, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndSByteUniqueSeq (1y, 1y, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Unique Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndSByteUniqueSeq (1y, 2y, 1)) 
            let b = [1y]
            Expect.equal a b "Expected equal"

        testCase "s Unique Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndSByteUniqueSeq (0y, 6y, 2)) 
            let b =[5y; 2y]
            Expect.equal a b "Expected equal"

        testCase "u Seq length of 9 consumes 2 64-bit cache blocks" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = List.ofSeq (x.RndByteSeq 9) 
            let b = [120uy; 22uy; 0uy; 17uy; 34uy; 16uy; 1uy; 51uy; 120uy]
            Expect.equal a b "Expected equal"

        testCase "u Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndByteSeq 0) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndByteSeq (1uy, 2uy, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "`u Seq range of max range" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq (x.RndByteSeq (System.Byte.MinValue, System.Byte.MaxValue, 255))).Length
            let b = 255
            Expect.equal a b "Expected equal"

        testCase "u Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndByteSeq (1uy, 2uy, 2))
            let b = [1uy; 1uy]
            Expect.equal a b "Expected equal"

        testCase "u Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndByteSeq (1uy, 1uy, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndByteSeq (0uy, 6uy, 2))
            let b = [5uy; 2uy]
            Expect.equal a b "Expected equal"

        testCase "u Seq returns predicted values" <| fun () ->
            let x = new RandomBits("7816000000000000")
            let a = List.ofSeq (x.RndByteSeq 2)
            let b = [120uy; 22uy]
            Expect.equal a b "Expected equal"

        testCase "u Unique Seq of length = range succeed" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq ( x.RndByteUniqueSeq (2uy, 12uy, 10))).Length 
            let b = 10
            Expect.equal a b "Expected equal"

        testCase "u Unique Seq of length gt range should fail" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> x.RndByteUniqueSeq (2uy, 12uy, 11) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndByteUniqueSeq (1uy, 2uy, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndByteUniqueSeq (1uy, 2uy, 1)) 
            let b = [1uy]
            Expect.equal a b "Expected equal"

        testCase "u Unique Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndByteUniqueSeq (1uy, 1uy, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Unique Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndByteUniqueSeq (0uy, 6uy, 2))
            let b = [5uy; 2uy]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"
        ]