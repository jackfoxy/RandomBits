module RandomBitsTest.FsCheck

open RandomBits
open Expecto
open FsCheck

//let formatter (o:obj) = box (o.ToString())
//let formatterRunner =
//   { new IRunner with
//       member x.OnStartFixture t =
//           printf "%s" (Runner.onStartFixtureToString t)
//       member x.OnArguments (ntest,args, every) =
//           printf "%s" (every ntest (args |> List.map formatter))
//       member x.OnShrink(args, everyShrink) =
//           printf "%s" (everyShrink (args |> List.map formatter))
//       member x.OnFinished(name,testResult) = 
//           let testResult' = match testResult with 
//                               | TestResult.False (testData,origArgs,shrunkArgs,outCome,seed) -> 
//                                   TestResult.False (testData,origArgs |> List.map formatter, shrunkArgs |> List.map formatter,outCome,seed)
//                               | t -> t
//           printf "%s" (Runner.onFinishedToString name testResult') 
//   }

//let private nUnitRunner =
//    { new IRunner with
//        member x.OnStartFixture t = ()
//        member x.OnArguments(ntest, args, every) = ()
//        member x.OnShrink(args, everyShrink) = ()
//        member x.OnFinished(name, result) = 
//            match result with 
//            | TestResult.True data -> 
//                printfn "%s" (Runner.onFinishedToString name result)
//            | _ -> Assert.Fail(Runner.onFinishedToString name result) }
   
//let private nUnitConfig = { Config.Default with Runner = nUnitRunner }

//let fsCheck name testable =
//    FsCheck.Check.One (name, nUnitConfig, testable)

//let rndByteSeqGen = 
//    gen {
////        let! inclLower = Arb.generate  //http://fscheck.codeplex.com/workitem/16796
//        let! inclLower = Gen.suchThat (fun x -> x < System.Byte.MaxValue) Arb.generate
//        let! exlUpper = Gen.suchThat (fun x -> x > inclLower) Arb.generate
//        let! count = Gen.suchThat (fun x -> x >= 1) Arb.generate
//        let x = new RandomBits("7816001122100133")
//        return (x.RndByteSeq (inclLower, exlUpper, count) |> List.ofSeq), (inclLower, exlUpper, count) 
//    }

//[<Test>]
//let ``byte seq range returns expected length`` () =

////    fsCheck "byte seq range length" (Prop.forAll (Arb.fromGen rndByteSeqGen) (fun ((myList : list<byte>), ((inclLower : byte), (exlUpper : byte), count)) -> myList.Length = count))
//    fsCheck "byte seq range length" (Prop.forAll (Arb.fromGen rndByteSeqGen) (fun ((myList : list<byte>), ((inclLower : byte), (exlUpper : byte), count)) -> Prop.classify (myList.Length = count) (sprintf "inclLower: %i, exlUpper: %i, count: %i" inclLower exlUpper count) (myList.Length = count)))

//let rndSByteSeqGen =  //signed byte not yet implemented in FsCheck 0.8.3.0
//    gen {
//        let! inclLower = Gen.suchThat (fun x -> x < System.SByte.MaxValue) Arb.generate
//        let! exlUpper = Gen.suchThat (fun x -> x > inclLower) Arb.generate
//        let! count = Gen.suchThat (fun x -> x >= 1) Arb.generate
//        let x = new RandomBits("7816001122100133")
//        return (x.RndSByteSeq (inclLower, exlUpper, count) |> List.ofSeq), (inclLower, exlUpper, count) 
//    }

////[<Test>]
////let ``s byte seq range returns expected length`` () =
////
////    fsCheck "s byte seq range length" (Prop.forAll (Arb.fromGen rndSByteSeqGen) (fun ((myList : list<sbyte>), ((inclLower : sbyte), (exlUpper : sbyte), count)) -> myList.Length = count))

