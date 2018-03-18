namespace RandomBits.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args Mock.mockTests |> ignore
        Tests.runTestsWithArgs defaultConfig args Int64.int64Tests |> ignore
        Tests.runTestsWithArgs defaultConfig args Int32.int32Tests |> ignore
        Tests.runTestsWithArgs defaultConfig args Int16.int16Tests |> ignore
        Tests.runTestsWithArgs defaultConfig args Byte.byteTests |> ignore
        Tests.runTestsWithArgs defaultConfig args Bool.boolTests |> ignore

        0

