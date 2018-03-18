module RandomBits.Tests.Int64

open RandomBits
open Expecto

[<Tests>]
let int64Tests =
    testList "Int64Tests" [

        testCase "random u and int64 from first mock byte" <| fun () ->
            let x = new RandomBits("7000000000000000")
            Expect.equal (x.RndUint64()) 8070450532247928832UL "Expected equal"
            Expect.equal (x.RndInt64()) 8070450532247928832L "Expected equal"
            Expect.equal x.Consume64Count 2L "Expected equal"

        testCase "random uint64 in range from first mock byte" <| fun () ->
            let x = new RandomBits("A800000000000000")
            let a = x.RndUint64 (0UL, 6UL) 
            let b = 5UL
            Expect.equal a b "Expected equal"

        testCase "random u and int64 in range of max range" <| fun () ->
            let x = new RandomBits("A800000000000000")
            Expect.equal (x.RndUint64 (System.UInt64.MinValue, System.UInt64.MaxValue)) 12105675798371893248UL "Expected equal"
            Expect.equal (x.RndInt64 (System.Int64.MinValue, System.Int64.MaxValue)) 2882303761517117440L "Expected equal"
            Expect.equal x.Consume64Count 2L "Expected equal"

        testCase "s Seq length of 8 consumes 8 64-bit cache block" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = List.ofSeq (x.RndInt64Seq 8) 
            let b = [8653103807624905011L; 8653103807624905011L; 8653103807624905011L; 8653103807624905011L; 8653103807624905011L; 8653103807624905011L; 8653103807624905011L; 8653103807624905011L]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 8L "Expected equal"

        testCase "s Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () ->List.ofSeq (x.RndInt64Seq 0) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq length of 9 consumes 9 64-bit cache blocks" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = List.ofSeq (x.RndUint64Seq 9) 
            let b = [8653103807624905011UL; 8653103807624905011UL; 8653103807624905011UL; 8653103807624905011UL; 8653103807624905011UL; 8653103807624905011UL; 8653103807624905011UL; 8653103807624905011UL; 8653103807624905011UL]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 9L "Expected equal"

        testCase "s Seq range length of less than 1 fails2" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndInt64Seq (1L, 2L, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt64Seq (1L, 2L, 2))
            let b = [1L; 1L]
            Expect.equal a b "Expected equal"

        testCase "s Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () ->List.ofSeq (x.RndInt64Seq (1L, 1L, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "s Seq range of max range" <| fun () ->
            let x = new RandomBits("7816001122100133")
            let a = (List.ofSeq (x.RndInt64Seq (System.Int64.MinValue, System.Int64.MaxValue, 255))).Length
            let b = 255
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 255L "Expected equal"

        testCase "s Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndInt64Seq (0L, 6L, 2)) 
            let b = [5L; 2L]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"

        testCase "u Seq length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint64Seq 0) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range length of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint64Seq (1UL, 2UL, 0)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range of less than 1 fails" <| fun () ->
            let x = new RandomBits("a800000000000000")
            Expect.throwsT<System.ArgumentException>
                (fun () -> List.ofSeq (x.RndUint64Seq (1UL, 1UL, 2)) |> ignore)
                "Expected System.ArgumentException"

        testCase "u Seq range of 1 returns predicted value" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint64Seq (1UL, 2UL, 2))
            let b = [1UL; 1UL]
            Expect.equal a b "Expected equal"
           
        testCase "u Seq range returns predicted values" <| fun () ->
            let x = new RandomBits("a800000000000000")
            let a = List.ofSeq (x.RndUint64Seq (0UL, 6UL, 2)) 
            let b = [5UL; 2UL]
            Expect.equal a b "Expected equal"
            Expect.equal x.Consume64Count 1L "Expected equal"
        ]