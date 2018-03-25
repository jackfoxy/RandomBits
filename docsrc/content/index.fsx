(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/RandomBits/net47"
#r "RandomBits.dll"
(**
RandomBits
======================

**RandomBits** is an F# .NET Standard library generating signed and unsigned 1, 8, 16, 32, and 64 bit random numbers from bits streamed from
the [Australian National University Quantum Random Numbers Server](http://qrng.anu.edu.au/index.php).

Signed and unsigned sequences
-----------------------------

Specify the length of the sequence.

*)
open RandomBits

let randomBits = new RandomBits()

let boolSeq = randomBits.RndBoolSeq(100)

printfn "%A" <| boolSeq
// seq [true; true; true; false; ...]

let signedByteSeq = randomBits.RndSByteSeq(10000)

printfn "%A" <| signedByteSeq
// seq [-7y; -46y; 39y; 121y; ...]

let unSigned64BitSeq = randomBits.RndUint64Seq(25)

printfn "%A" <| unSigned64BitSeq
// seq [1906034160960843131UL; 6192668109370953241UL; 4558849705110384999UL; 866484679755652446UL; ...]

(**
Random signed and unsigned numbers
----------------------------------
*)
let randomBool = randomBits.RndBool()
printfn "%b" <| randomBool
// false

let randomUnsignedByte = randomBits.RndByte()
printfn "%A" <| randomUnsignedByte
// 211uy

let randomSigned16Bit = randomBits.RndInt16()
printfn "%A" <| randomSigned16Bit
// 1628s

let randomUnsigned32Bit = randomBits.RndUint32()
printfn "%i" <| randomUnsigned32Bit
// 3485950116

let randomeSigned64Bit = randomBits.RndInt64()
printfn "%i" <| randomeSigned64Bit
// 592053077296867471

(**
Random numbers constrained within a range
-----------------------------------------

Inclusive lower and exclusive upper bound.
*)
let randomConstrainedByte = randomBits.RndSByte(50y, 100y)
printfn "%A" <| randomConstrainedByte
// 80y

let randomConstrainedU64Int = randomBits.RndUint64(1000000000000UL, 1000000000200UL)
printfn "%A" <| randomConstrainedU64Int
// 1000000000091UL

(**
Sequences of random numbers constrained within a range
------------------------------------------------------

Inclusive lower, exclusive upper bound, and sequence length.
*)
let randomConstrainedInt32Seq = randomBits.RndInt32Seq(256, 1024, 100)
printfn "%A" <| randomConstrainedInt32Seq
// seq [505; 954; 598; 280; ...]

(**
Sequences of random numbers constrained within a range, each member of the sequence unique
------------------------------------------------------------------------------------------

Inclusive lower, exclusive upper bound, and sequence length.
*)
let randomUniqeConstrainedInt16Seq = randomBits.RndInt16UniqueSeq(256s, 1024s, 200)
printfn "%A" <| randomUniqeConstrainedInt16Seq
// seq [266s; 665s; 457s; 392s; ...]

(**
Samples & documentation
-----------------------

 * A sample .Net 4.7 and .Net Core 2.0 [application is available](https://github.com/jackfoxy/RandomBits/tree/master/src/LottoMonteCarlo). It runs Monte Carlo simulations of a Lotto game.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

You can [report issues][issues], fork the project, and submit pull requests. Please also 
add tests and samples that can be turned into [documentation](https://github.com/jackfoxy/RandomBits/tree/master/docsrc/content).

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/jackfoxy/RandomBits/tree/master/docs/content
  [gh]: https://github.com/jackfoxy/RandomBits
  [issues]: https://github.com/jackfoxy/RandomBits/issues
  [readme]: https://github.com/jackfoxy/RandomBits/blob/master/README.md
  [license]: https://github.com/jackfoxy/RandomBits/blob/master/LICENSE.txt
*)
