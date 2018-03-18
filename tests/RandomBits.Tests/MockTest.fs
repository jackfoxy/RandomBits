module RandomBits.Tests.Mock

open RandomBits
open Expecto

[<Tests>]
let mockTests =
    testList "mockTests" [

        testCase "all hex chars work in mock string" <| fun () ->
            let x = new RandomBits("1234567890abcdefABCDEF0000000000")
            Expect.equal x.CacheLength 2 "Expected equal"

        testCase "non 64-bit mock string lengths truncate" <| fun () ->
            let x = new RandomBits("1234567890abcdefABCDEF000000000")
            Expect.equal x.CacheLength 1 "Expected equal"

            let x = new RandomBits("1234567890abcdefABCDEF00000000011")
            Expect.equal x.CacheLength 2 "Expected equal"

        testCase "non-hex mock string should fail" <| fun () ->
            Expect.throwsT<System.Exception>
               (fun () -> (new RandomBits("800000000000000v")) |> ignore)
                "Expected System.Exception"

        testCase "too short mock string should fail" <| fun () ->
            Expect.throwsT<System.Exception>
               (fun () -> (new RandomBits("800000000000000")) |> ignore)
                "Expected System.Exception"
        ]