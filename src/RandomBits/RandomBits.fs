﻿namespace RandomBits

open FSharp.Data
open System
open System.Collections.Concurrent
open System.Net
open System.Numerics
open FSharpx.Collections

type private AnuJSON = JsonProvider<"""{"type":"string","length":100,"size":100,"data":["9b","b2"],"success":true}""">

/// Source of random bit stream from ANU.
type RandomBits =
    val private anuBlockCount : int  //number of 1024 byte blocks per request to ANU
    val private anuUrl : string
    val mutable private consume64Count : int64
    val private theMock : string
    val private theQueue : ConcurrentQueue<uint64>

    val mutable private bitSource : uint64
    val mutable private ptr : uint64
 
    /// constructor for streaming bits from ANU
    new () as __ =
        {anuBlockCount = 32;
        anuUrl = "https://qrng.anu.edu.au/API/jsonI.php";
        consume64Count = 0L;
        theMock = "";
        theQueue = ConcurrentQueue<uint64>();
        bitSource = 0UL;
        ptr = 0UL;
         }

        then
        RandomBits.retrieveBits __.theQueue __.anuUrl __.anuBlockCount ""

    /// constructor for testing
    /// mock string representing at least one hexadecimal 64 bit unsigned integer
    new (mock) as __ =
        {anuBlockCount = 0;
        anuUrl = "";
        consume64Count = 0L;
        theMock = "\"" + mock + "\"";
        theQueue = ConcurrentQueue<uint64>();
        bitSource = 0UL;
        ptr = 0UL;
         }

        then
        if __.theMock.Length < 18 then failwith "Mock string must represent at least one 64-bit integer"

        let l = Array.fold (fun l t -> match t with
                                        | '0' -> 0u::l
                                        | '1' -> 1u::l
                                        | '2' -> 2u::l
                                        | '3' -> 3u::l
                                        | '4' -> 4u::l
                                        | '5' -> 5u::l
                                        | '6' -> 6u::l
                                        | '7' -> 7u::l
                                        | '8' -> 8u::l
                                        | '9' -> 9u::l
                                        | 'a' | 'A' -> 10u::l
                                        | 'b' | 'B' -> 11u::l
                                        | 'c' | 'C' -> 12u::l
                                        | 'd' | 'D' -> 13u::l
                                        | 'e' | 'E' -> 14u::l
                                        | 'f' | 'F' -> 15u::l
                                        | c -> failwith (c.ToString() + " not a hex character in mock.") ) [] (mock.ToCharArray())
                                        //seems to somehow not throw out of the two anon funs interfaceretrieveBits, so repeating it here

        RandomBits.retrieveBits __.theQueue "" 0 __.theMock

    static member private retrieveBits (theQueue : ConcurrentQueue<uint64>) (anuUrl : string) (anuBlockCount : int) (mock : string) =
        use webClient = new WebClient()

        let f = (fun n ->
            let nn = n.ToString()
            let nnn = nn.Substring(1, (nn.Length - 2))  //lose quote marks around string
            let l = Array.fold (fun l t -> match t with
                                            | '0' -> 0u::l
                                            | '1' -> 1u::l
                                            | '2' -> 2u::l
                                            | '3' -> 3u::l
                                            | '4' -> 4u::l
                                            | '5' -> 5u::l
                                            | '6' -> 6u::l
                                            | '7' -> 7u::l
                                            | '8' -> 8u::l
                                            | '9' -> 9u::l
                                            | 'a' | 'A' -> 10u::l
                                            | 'b' | 'B' -> 11u::l
                                            | 'c' | 'C' -> 12u::l
                                            | 'd' | 'D' -> 13u::l
                                            | 'e' | 'E' -> 14u::l
                                            | 'f' | 'F' -> 15u::l
                                            | c -> failwith (c.ToString() + " not a hex character in cache feed.") ) [] (nnn.ToCharArray())
                                            //seems to somehow not throw out of the two anon funs
            let rec loop acc = function
                | h1 :: h2 :: tl -> loop ((h1 |||  (h2  <<< 4)) :: acc) tl
                | _ -> acc
                
            let lBytes = loop [] l  //not really bytes, since they are int32
                 
            let rec loop2 = function
                | h0 :: h1 :: h2 :: h3 :: h4 :: h5 :: h6 :: h7 :: tl -> 
                    theQueue.Enqueue ( ((uint64 h0) <<< 56) ||| ((uint64 h1) <<< 48)  ||| ((uint64 h2) <<< 40)  ||| ((uint64 h3) <<< 32)  
                                        ||| ((uint64 h4) <<< 24) ||| ((uint64 h5) <<< 16)  ||| ((uint64 h6) <<< 8)  ||| (uint64 h7) )
                    loop2 tl
                | _ -> ()
            loop2 lBytes)

        try
            if mock.Length > 0 then 
                Seq.iter f (seq {yield mock})
            else 
                Seq.iter f (AnuJSON.Parse (webClient.DownloadString (anuUrl + "?type=hex16&length=" + anuBlockCount.ToString() + "&size=1024"))).Data
        with
            | _ -> ()

    member private __.next64() =

        let (success, ru64) = __.theQueue.TryDequeue()
        
        if success then 
            __.consume64Count <- __.consume64Count + 1L
            ru64
        else 
            RandomBits.retrieveBits __.theQueue __.anuUrl __.anuBlockCount  __.theMock
            let (success, ru64) = __.theQueue.TryDequeue()
            __.consume64Count <- __.consume64Count + 1L
            ru64                             

    member private __.nextBit() =
        
        __.ptr <- __.ptr >>> 1
        if __.ptr = 0UL then
            __.bitSource <- __.next64()
            __.ptr <- 9223372036854775808UL

        ((__.ptr &&& __.bitSource) = __.ptr)

    static member inline private unsignedOfBit displ bitOffset bit64 bitSize (zero : 'a) (one : 'a) =     //inline to make bitwise operator generic

        let x' = (1UL <<< (64 - displ - bitOffset))
        if (x' &&& bit64) = x' then one <<< (bitSize - bitOffset)
        else zero

    member inline private __.bitsToNumber bitSize (zero : 'a) (one : 'a) =    //inline to make bitwise operator generic

        let bit64 = __.next64()

        let rec loop acc offset = 
            if offset > bitSize then acc
            else loop (acc ||| (RandomBits.unsignedOfBit 0 offset bit64 bitSize zero one)) (offset + 1)
        
        loop (RandomBits.unsignedOfBit 0 1 bit64 bitSize zero one) 2

    member inline private __.bitsToNumber2 bitSize (zero : 'a) (one : 'a) bit64 displ =    //inline to make bitwise operator generic

        let rec loop acc offset = 
            if offset > bitSize then acc
            else loop (acc ||| (RandomBits.unsignedOfBit displ offset bit64 bitSize zero one)) (offset + 1)
        
        loop (RandomBits.unsignedOfBit displ 1 bit64 bitSize zero one) 2

    member inline private  __.randomWalk (lower : 'a) (upper : 'a) (zero : 'a) (one : 'a) =

        //bit shifts to determine highest order bit of upper (power)
        let pwr =       
            let rec loop u acc =
                match (u >>> 1) with
                | u' when u' > zero -> loop u' (acc + 1)
                | zero -> acc
            loop upper 0

        //random walk down to array of smallest power of 2 including upper
        let rec loop acc pwrDec =
            match pwrDec with
            | -1 -> acc
            | _ ->
                if __.nextBit() then loop (acc +  (one <<< pwrDec)) (pwrDec - 1)
                else loop acc (pwrDec - 1)

        //greater than upper? try again
        let rec loop2 = function
            | x when x > upper -> loop2 (loop zero pwr)
            | x -> x

        let myRnd = loop2 (loop zero pwr)

        myRnd + lower

    member inline private  __.rndInRange (inclLower : 'a) (exlUpper : 'a) (zero : 'a) (one : 'a) =
        let range =  exlUpper -  inclLower 
        if range < one then failwith "range must be greater than 1"

        __.randomWalk inclLower (exlUpper - (inclLower + one)) zero one

    member inline private  __.rndSignInRange (inclLower : 'a) (exlUpper : 'a) (uZero : 'c) (nextOne : 'b) (nextSigned : 'a -> 'b) 
        (nextSigned2 : 'c -> 'b) (signed : 'b -> 'a) (unsigned : 'b -> 'c)  (uConv : ('c * 'c) -> 'c) = 

        let lower =  (nextSigned inclLower)
        let range =  (nextSigned exlUpper) - lower
        if range < nextOne then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)

        signed (nextSigned2 (uConv (uZero, (unsigned range))) + lower)

    member inline private __.rndSeq length bitSize (zero : 'a) (one : 'a) (signed : 'a -> 'b) = 

        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        let ptr = ref length 

        seq {while !ptr > 0 do
                let bit64 = __.next64()

                let n' =
                    if !ptr > (64 / bitSize) then (64 / bitSize)
                    else !ptr

                for i = 1 to n' do     
                    yield signed (__.bitsToNumber2 bitSize zero one bit64 ((i - 1) * bitSize))

                ptr := !ptr - n'
             }

    member inline private __.rndSignRangeSeq (inclLower : 'a) (exlUpper : 'a) length (nextZero : 'b) (nextOne : 'b)  (nextSigned : 'a -> 'b) (signed : 'b -> 'a) = 
        let range =  (nextSigned exlUpper) - (nextSigned inclLower)
        if range < nextOne then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)
        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        let lower = nextSigned inclLower
        let upper = (nextSigned exlUpper) - (lower + nextOne)

        seq {for i = 1 to length do
                yield (signed (__.randomWalk lower upper nextZero nextOne))
             }

    member inline private __.rndUnsignRangeSeq (inclLower : 'a) (exlUpper : 'a) length (zero : 'a) (one : 'a)  = 

        let range =  exlUpper -  inclLower 
        if range < one then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)
        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        seq {for i = 1 to length do
                yield __.rndInRange inclLower exlUpper zero one
            }

    member inline private __.rndSignUniqueSeq (inclLower : 'a) (exlUpper : 'a) length  (nextZero : 'b) (nextOne : 'b) (nextSigned : 'a -> 'b) (signed : 'b -> 'a) = 
        
        let lower = nextSigned inclLower
        let upper = nextSigned exlUpper
        let range =  upper - lower  
        if range < nextOne then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)
        if (uint64 length) > (uint64 range) then invalidArg "length" (sprintf "seq length %i cannot be greater than range %i - %i of unique values" length inclLower exlUpper)
        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)
        
        let count = ref nextZero
        let h = ref (Heap.empty false)
        
        seq {for i = 1 to length do

                let x = __.randomWalk lower (upper - !count - (lower + nextOne)) nextZero nextOne

                let x' = Seq.fold (fun x t -> 
                                    if x >= t then x + nextOne
                                    else x) x (!h)
                yield signed x'
                count := !count + nextOne
                h := (!h).Insert x'
            }

    member inline private __.rndUnsignUniqueSeq (inclLower : 'a) (exlUpper : 'a) length (zero : 'a) (one : 'a) = 

        let range =  exlUpper -  inclLower 
        if range < one then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)
        if (uint64 length) > (uint64 range) then invalidArg "length" (sprintf "seq length %i cannot be greater than range %i - %i of unique values" length inclLower exlUpper)
        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        let count = ref zero
        let h = ref (Heap.empty false)
        
        seq {for i = 1 to length do

                let x = __.randomWalk inclLower (exlUpper - !count - (inclLower + one)) zero one

                let x' = Seq.fold (fun x t -> 
                                    if x >= t then x + one
                                    else x) x (!h)
                yield x'
                count := !count + one
                h := (!h).Insert x'
            }

    member inline private __.bigint (x : int64) = BigInteger(x)

    member inline private __.bigintU (x : uint64) = BigInteger(x)

    /// count of unsigned 16 bit numbers to return from ANU on each call
    member __.ANU_BlockCount = __.anuBlockCount

    /// url to the ANU JSON API
    member __.ANU_Url = __.anuUrl

    /// count of unsigned 64 bit integers currently cached
    member __.CacheLength = __.theQueue.Count

    /// count of unsigned 64 bit integer consumed
    member __.Consume64Count = __.consume64Count

    (* random numbers *)

    ///random bool
    member __.RndBool() =
        let x = __.next64()

        let x' = (1UL <<< 63)
        if (x' &&& x) = x' then true
        else false

    ///random signed 8-bit integer
    member __.RndSByte() = sbyte (__.bitsToNumber 8 0uy 1uy)

    ///random unsigned 8-bit integer
    member __.RndByte() = __.bitsToNumber 8 0uy 1uy

    ///random signed 16-bit integer
    member __.RndInt16() = int16 (__.bitsToNumber 16 0us 1us)

    ///random unsigned 16-bit integer
    member __.RndUint16() = __.bitsToNumber 16 0us 1us 

    ///random signed 32-bit integer
    member __.RndInt32() = int32 (__.bitsToNumber 32 0 1)

    ///random unsigned 32-bit integer
    member __.RndUint32() = __.bitsToNumber 32 0u 1u

    ///random signed 64-bit integer
    member __.RndInt64() = int64 (__.next64())

    ///random unsigned 64-bit integer
    member __.RndUint64() = __.next64()

    (* random numbers in range *)

    ///random signed 8-bit integer in range inclusive of lower and exclusive of upper
    member __.RndSByte (inclLower, exlUpper) = __.rndSignInRange inclLower exlUpper 0uy 1s (int16) (int16) (sbyte) (byte) (__.RndByte)

    ///random unsigned 8-bit integer in range inclusive of lower and exclusive of upper
    member __.RndByte (inclLower, exlUpper) = __.rndInRange inclLower exlUpper 0uy 1uy

    ///random signed 16-bit integer in range inclusive of lower and exclusive of upper
    member __.RndInt16 (inclLower, exlUpper) = __.rndSignInRange inclLower exlUpper 0us 1 (int32) (int32) (int16) (uint16) (__.RndUint16)

    ///random unsigned 16-bit integer in range inclusive of lower and exclusive of upper
    member __.RndUint16 (inclLower, exlUpper) = __.rndInRange inclLower exlUpper 0us 1us

    ///random signed 32-bit integer in range inclusive of lower and exclusive of upper
    member __.RndInt32 (inclLower, exlUpper) = __.rndSignInRange inclLower exlUpper 0u 1L (int64) (int64) (int32) (uint32) (__.RndUint32)

    ///random unsigned 32-bit integer in range inclusive of lower and exclusive of upper
    member __.RndUint32 (inclLower, exlUpper) = __.rndInRange inclLower exlUpper 0u 1u

    ///random signed 64-bit integer in range inclusive of lower and exclusive of upper
    member __.RndInt64 (inclLower, exlUpper) = 

        let lower =  (__.bigint inclLower)
        let range =  (__.bigint exlUpper) - lower
        if range < 1I then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)

        int64 (__.bigintU (__.RndUint64 (0UL, (uint64 range))) + lower)

    ///random unsigned 64-bit integer in range inclusive of lower and exclusive of upper
    member __.RndUint64 (inclLower, exlUpper) = __.rndInRange inclLower exlUpper 0UL 1UL

    (* random sequences *)
    
    ///random bool seq of length
    member __.RndBoolSeq length =

        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        seq {for i = 1 to length do
                yield __.nextBit()
            }

    ///random signed 8-bit integer seq of length
    member __.RndSByteSeq length = __.rndSeq length 8 0y 1y (sbyte)

    ///random unsigned 8-bit integer seq of length
    member __.RndByteSeq length = __.rndSeq length 8 0uy 1uy (byte)

    ///random signed 16-bit integer seq of length
    member __.RndInt16Seq length = __.rndSeq length 16 0s 1s (int16)

    ///random unsigned 16-bit integer seq of length
    member __.RndUint16Seq length = __.rndSeq length 16 0us 1us (uint16)

    ///random signed 32-bit integer seq of length
    member __.RndInt32Seq length = __.rndSeq length 32 0 1 (int)

    ///random unsigned 32-bit integer seq of length
    member __.RndUint32Seq length = __.rndSeq length 32 0u 1u (uint32)

    ///random signed 64-bit integer seq of length
    member __.RndInt64Seq length = __.rndSeq length 64 0L 1L (int64)

    ///random unsigned 64-bit integer seq of length
    member __.RndUint64Seq length = __.rndSeq length 64 0UL 1UL (uint64)

    (* random numbers in range sequences *)

    ///random signed 8-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndSByteSeq (inclLower, exlUpper, length)  = __.rndSignRangeSeq inclLower exlUpper length 0s 1s (int16) (sbyte)

    ///random unsigned 8-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndByteSeq (inclLower, exlUpper, length)  = __.rndUnsignRangeSeq inclLower exlUpper length 0uy 1uy

    ///random signed 16-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndInt16Seq (inclLower, exlUpper, length)  = __.rndSignRangeSeq inclLower exlUpper length 0 1 (int32) (int16)
    
    ///random unsigned 16-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndUint16Seq (inclLower, exlUpper, length)  = __.rndUnsignRangeSeq inclLower exlUpper length 0us 1us

    ///random signed 32-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndInt32Seq (inclLower, exlUpper, length)  = __.rndSignRangeSeq inclLower exlUpper length 0L 1L (int64) (int)

    ///random unsigned 32-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndUint32Seq (inclLower, exlUpper, length)  = __.rndUnsignRangeSeq inclLower exlUpper length 0u 1u

    ///random signed 64-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndInt64Seq ((inclLower : int64), (exlUpper : int64), length)  = __.rndSignRangeSeq inclLower exlUpper length 0I 1I (__.bigint) (int64)

    ///random unsigned 64-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndUint64Seq (inclLower, exlUpper, length)  = __.rndUnsignRangeSeq inclLower exlUpper length 0UL 1UL

    (* unique random numbers in range sequences *)

    ///random unique usigned 8-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndSByteUniqueSeq (inclLower, exlUpper, length)  = __.rndSignUniqueSeq inclLower exlUpper length 0s 1s (int16) (sbyte)

    ///random unique unsigned 8-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndByteUniqueSeq (inclLower, exlUpper, length)  = __.rndUnsignUniqueSeq inclLower exlUpper length 0uy 1uy

    ///random unique signed 16-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndInt16UniqueSeq (inclLower, exlUpper, length)  = __.rndSignUniqueSeq inclLower exlUpper length 0 1 (int32) (int16)

    ///random unique unsigned 16-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndUint16UniqueSeq (inclLower, exlUpper, length)  = __.rndUnsignUniqueSeq inclLower exlUpper length 0us 1us

    ///random unique signed 32-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndInt32UniqueSeq (inclLower, exlUpper, length)  = __.rndSignUniqueSeq inclLower exlUpper length 0L 1L (int64) (int)

    ///random unique unsigned 32-bit integer in range inclusive of lower and exclusive of upper seq of length
    member __.RndUint32UniqueSeq (inclLower, exlUpper, length)  = __.rndUnsignUniqueSeq inclLower exlUpper length 0u 1u
    
        

    with
    interface IDisposable with
        member __.Dispose() = ()

