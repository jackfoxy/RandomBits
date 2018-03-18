namespace RandomBits

open FSharp.Data
open System
open System.Collections.Concurrent
open System.Net
open System.Numerics
open FSharpx.Collections
//open FSharpx.DataStructures
//open FSharpx.TypeProviders.Documents

type private AnuJSON = JsonProvider<"""{"type":"string","length":100,"size":100,"data":["9b","b2"],"success":true}""">

type RandomBits =
    val private anuBlockCount : int  //number of 1024 byte blocks per request to ANU
    val private anuUrl : string
    val mutable private consume64Count : int64
    val private maxCache : int
    val private maxPersistCache : int
    val private persistPath : string
    val private reusePeristCache : bool
    val private theMock : string
    val private theQueue : ConcurrentQueue<uint64>

    val mutable private bitSource : uint64
    val mutable private ptr : uint64
 
    static member private retrieveBits (theQueue : ConcurrentQueue<uint64>) (anuUrl : string) (anuBlockCount : int) (mock : string) =
        use webClient = new WebClient()
        try 
            let x = 
                if mock.Length > 0 then 
                    ""
                else 
                    webClient.DownloadString (anuUrl + "?type=hex16&length=" + anuBlockCount.ToString() + "&size=1024")
            if (mock.Length > 0) || (AnuJSON.Parse x).Success then 
                
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

                if mock.Length > 0 then 
                    Seq.iter f (seq {yield mock})
                else 
                    Seq.iter f (AnuJSON.Parse x).Data
                
                ()
            else ()
        with
            | _ -> ()

    member private this.next64() =

        let (success, ru64) = this.theQueue.TryDequeue()
        
        if success then 
            this.consume64Count <- this.consume64Count + 1L
            ru64
        else 
            RandomBits.retrieveBits this.theQueue this.anuUrl this.anuBlockCount  this.theMock
            let (success, ru64) = this.theQueue.TryDequeue()
            this.consume64Count <- this.consume64Count + 1L
            ru64                             

    member private this.nextBit() =
        
        this.ptr <- this.ptr >>> 1
        if this.ptr = 0UL then
            this.bitSource <- this.next64()
            this.ptr <- 9223372036854775808UL

        ((this.ptr &&& this.bitSource) = this.ptr)

    static member inline private unsignedOfBit displ bitOffset bit64 bitSize (zero : 'a) (one : 'a) =     //inline to make bitwise operator generic

        let x' = (1UL <<< (64 - displ - bitOffset))
        if (x' &&& bit64) = x' then one <<< (bitSize - bitOffset)
        else zero

    member inline private this.bitsToNumber bitSize (zero : 'a) (one : 'a) =    //inline to make bitwise operator generic

        let bit64 = this.next64()

        let rec loop acc offset = 
            if offset > bitSize then acc
            else loop (acc ||| (RandomBits.unsignedOfBit 0 offset bit64 bitSize zero one)) (offset + 1)
        
        loop (RandomBits.unsignedOfBit 0 1 bit64 bitSize zero one) 2

    member inline private this.bitsToNumber2 bitSize (zero : 'a) (one : 'a) bit64 displ =    //inline to make bitwise operator generic

        let rec loop acc offset = 
            if offset > bitSize then acc
            else loop (acc ||| (RandomBits.unsignedOfBit displ offset bit64 bitSize zero one)) (offset + 1)
        
        loop (RandomBits.unsignedOfBit displ 1 bit64 bitSize zero one) 2

    member inline private  this.randomWalk (lower : 'a) (upper : 'a) (zero : 'a) (one : 'a) =

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
                if this.nextBit() then loop (acc +  (one <<< pwrDec)) (pwrDec - 1)
                else loop acc (pwrDec - 1)

        //greater than upper? try again
        let rec loop2 = function
            | x when x > upper -> loop2 (loop zero pwr)
            | x -> x

        let myRnd = loop2 (loop zero pwr)

        myRnd + lower

    member inline private  this.rndInRange (inclLower : 'a) (exlUpper : 'a) (zero : 'a) (one : 'a) =
        let range =  exlUpper -  inclLower 
        if range < one then failwith "range must be greater than 1"

        this.randomWalk inclLower (exlUpper - (inclLower + one)) zero one

    member inline private  this.rndSignInRange (inclLower : 'a) (exlUpper : 'a) (uZero : 'c) (nextOne : 'b) (nextSigned : 'a -> 'b) 
        (nextSigned2 : 'c -> 'b) (signed : 'b -> 'a) (unsigned : 'b -> 'c)  (uConv : ('c * 'c) -> 'c) = 

        let lower =  (nextSigned inclLower)
        let range =  (nextSigned exlUpper) - lower
        if range < nextOne then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)

        signed (nextSigned2 (uConv (uZero, (unsigned range))) + lower)

    member inline private this.rndSeq length bitSize (zero : 'a) (one : 'a) (signed : 'a -> 'b) = 

        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        let ptr = ref length 

        seq {while !ptr > 0 do
                let bit64 = this.next64()

                let n' =
                    if !ptr > (64 / bitSize) then (64 / bitSize)
                    else !ptr

                for i = 1 to n' do     
                    yield signed (this.bitsToNumber2 bitSize zero one bit64 ((i - 1) * bitSize))

                ptr := !ptr - n'
             }

    member inline private this.rndSignRangeSeq (inclLower : 'a) (exlUpper : 'a) length (nextZero : 'b) (nextOne : 'b)  (nextSigned : 'a -> 'b) (signed : 'b -> 'a) = 
        let range =  (nextSigned exlUpper) - (nextSigned inclLower)
        if range < nextOne then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)
        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        let lower = nextSigned inclLower
        let upper = (nextSigned exlUpper) - (lower + nextOne)

        seq {for i = 1 to length do
                yield (signed (this.randomWalk lower upper nextZero nextOne))
             }

    member inline private this.rndUnsignRangeSeq (inclLower : 'a) (exlUpper : 'a) length (zero : 'a) (one : 'a)  = 

        let range =  exlUpper -  inclLower 
        if range < one then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)
        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        seq {for i = 1 to length do
                yield this.rndInRange inclLower exlUpper zero one
            }

    member inline private this.rndSignUniqueSeq (inclLower : 'a) (exlUpper : 'a) length  (nextZero : 'b) (nextOne : 'b) (nextSigned : 'a -> 'b) (signed : 'b -> 'a) = 
        
        let lower = nextSigned inclLower
        let upper = nextSigned exlUpper
        let range =  upper - lower  
        if range < nextOne then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)
        if (uint64 length) > (uint64 range) then invalidArg "length" (sprintf "seq length %i cannot be greater than range %i - %i of unique values" length inclLower exlUpper)
        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)
        
        let count = ref nextZero
        let h = ref (Heap.empty false)
        
        seq {for i = 1 to length do

                let x = this.randomWalk lower (upper - !count - (lower + nextOne)) nextZero nextOne

                let x' = Seq.fold (fun x t -> 
                                    if x >= t then x + nextOne
                                    else x) x (!h)
                yield signed x'
                count := !count + nextOne
                h := (!h).Insert x'
            }

    member inline private this.rndUnsignUniqueSeq (inclLower : 'a) (exlUpper : 'a) length (zero : 'a) (one : 'a) = 

        let range =  exlUpper -  inclLower 
        if range < one then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)
        if (uint64 length) > (uint64 range) then invalidArg "length" (sprintf "seq length %i cannot be greater than range %i - %i of unique values" length inclLower exlUpper)
        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        let count = ref zero
        let h = ref (Heap.empty false)
        
        seq {for i = 1 to length do

                let x = this.randomWalk inclLower (exlUpper - !count - (inclLower + one)) zero one

                let x' = Seq.fold (fun x t -> 
                                    if x >= t then x + one
                                    else x) x (!h)
                yield x'
                count := !count + one
                h := (!h).Insert x'
            }

    member inline private this.bigint (x : int64) = BigInteger(x)

    member inline private this.bigintU (x : uint64) = BigInteger(x)

    new () as this =
        {anuBlockCount = 32;
        anuUrl = "https://qrng.anu.edu.au/API/jsonI.php";
        consume64Count = 0L;
        maxCache = 4096;
        maxPersistCache = 0;
        persistPath = "";
        reusePeristCache = false;
        theMock = "";
        theQueue = ConcurrentQueue<uint64>();
        bitSource = 0UL;
        ptr = 0UL;
         }

        then
        RandomBits.retrieveBits this.theQueue this.anuUrl this.anuBlockCount ""

    new (mock) as this =
        {anuBlockCount = 0;
        anuUrl = "";
        maxCache = 1;
        maxPersistCache = 0;
        consume64Count = 0L;
        persistPath = "";
        reusePeristCache = false;
        theMock = "\"" + mock + "\"";
        theQueue = ConcurrentQueue<uint64>();
        bitSource = 0UL;
        ptr = 0UL;
         }

        then
        if this.theMock.Length < 18 then failwith "Mock string must represent at least one 64-bit integer"

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

        RandomBits.retrieveBits this.theQueue "" 0 this.theMock

    member this.ANU_BlockCount = this.anuBlockCount
    member this.ANU_Url = this.anuUrl
    member this.CacheLength = this.theQueue.Count
    member this.Consume64Count = this.consume64Count
    member this.MaxCache = this.maxCache
    member this.MaxPersistCache = this.maxPersistCache
    member this.PersistCacheLength = 0
    member this.PersistPath = this.persistPath
    member this.ReusePeristCache = this.reusePeristCache

    (* random numbers *)

    ///random bool
    member this.RndBool() =
        let x = this.next64()

        let x' = (1UL <<< 63)
        if (x' &&& x) = x' then true
        else false

    ///random signed 8-bit integer
    member this.RndSByte() = sbyte (this.bitsToNumber 8 0uy 1uy)

    ///random unsigned 8-bit integer
    member this.RndByte() = this.bitsToNumber 8 0uy 1uy

    ///random signed 16-bit integer
    member this.RndInt16() = int16 (this.bitsToNumber 16 0us 1us)

    ///random unsigned 16-bit integer
    member this.RndUint16() = this.bitsToNumber 16 0us 1us 

    ///random signed 32-bit integer
    member this.RndInt32() = int32 (this.bitsToNumber 32 0 1)

    ///random unsigned 32-bit integer
    member this.RndUint32() = this.bitsToNumber 32 0u 1u

    ///random signed 64-bit integer
    member this.RndInt64() = int64 (this.next64())

    ///random unsigned 64-bit integer
    member this.RndUint64() = this.next64()

    (* random numbers in range *)

    ///random signed 8-bit integer in range inclusive of lower and exclusive of upper
    member this.RndSByte (inclLower, exlUpper) = this.rndSignInRange inclLower exlUpper 0uy 1s (int16) (int16) (sbyte) (byte) (this.RndByte)

    ///random unsigned 8-bit integer in range inclusive of lower and exclusive of upper
    member this.RndByte (inclLower, exlUpper) = this.rndInRange inclLower exlUpper 0uy 1uy

    ///random signed 16-bit integer in range inclusive of lower and exclusive of upper
    member this.RndInt16 (inclLower, exlUpper) = this.rndSignInRange inclLower exlUpper 0us 1 (int32) (int32) (int16) (uint16) (this.RndUint16)

    ///random unsigned 16-bit integer in range inclusive of lower and exclusive of upper
    member this.RndUint16 (inclLower, exlUpper) = this.rndInRange inclLower exlUpper 0us 1us

    ///random signed 32-bit integer in range inclusive of lower and exclusive of upper
    member this.RndInt32 (inclLower, exlUpper) = this.rndSignInRange inclLower exlUpper 0u 1L (int64) (int64) (int32) (uint32) (this.RndUint32)

    ///random unsigned 32-bit integer in range inclusive of lower and exclusive of upper
    member this.RndUint32 (inclLower, exlUpper) = this.rndInRange inclLower exlUpper 0u 1u

    ///random signed 64-bit integer in range inclusive of lower and exclusive of upper
    member this.RndInt64 (inclLower, exlUpper) = 

        let lower =  (this.bigint inclLower)
        let range =  (this.bigint exlUpper) - lower
        if range < 1I then invalidArg  "range" (sprintf "%i %i range must be greater than 0" inclLower exlUpper)

        int64 (this.bigintU (this.RndUint64 (0UL, (uint64 range))) + lower)

    ///random unsigned 64-bit integer in range inclusive of lower and exclusive of upper
    member this.RndUint64 (inclLower, exlUpper) = this.rndInRange inclLower exlUpper 0UL 1UL

    (* random sequences *)
    
    ///random bool seq of length
    member this.RndBoolSeq length =

        if length < 1 then invalidArg  "length" (sprintf "%i must be greater than 0" length)

        seq {for i = 1 to length do
                yield this.nextBit()
            }

    ///random signed 8-bit integer seq of length
    member this.RndSByteSeq length = this.rndSeq length 8 0y 1y (sbyte)

    ///random unsigned 8-bit integer seq of length
    member this.RndByteSeq length = this.rndSeq length 8 0uy 1uy (byte)

    ///random signed 16-bit integer seq of length
    member this.RndInt16Seq length = this.rndSeq length 16 0s 1s (int16)

    ///random unsigned 16-bit integer seq of length
    member this.RndUint16Seq length = this.rndSeq length 16 0us 1us (uint16)

    ///random signed 32-bit integer seq of length
    member this.RndInt32Seq length = this.rndSeq length 32 0 1 (int)

    ///random unsigned 32-bit integer seq of length
    member this.RndUint32Seq length = this.rndSeq length 32 0u 1u (uint32)

    ///random signed 64-bit integer seq of length
    member this.RndInt64Seq length = this.rndSeq length 64 0L 1L (int64)

    ///random unsigned 64-bit integer seq of length
    member this.RndUint64Seq length = this.rndSeq length 64 0UL 1UL (uint64)

    (* random numbers in range sequences *)

    ///random signed 8-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndSByteSeq (inclLower, exlUpper, length)  = this.rndSignRangeSeq inclLower exlUpper length 0s 1s (int16) (sbyte)

    ///random unsigned 8-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndByteSeq (inclLower, exlUpper, length)  = this.rndUnsignRangeSeq inclLower exlUpper length 0uy 1uy

    ///random signed 16-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndInt16Seq (inclLower, exlUpper, length)  = this.rndSignRangeSeq inclLower exlUpper length 0 1 (int32) (int16)
    
    ///random unsigned 16-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndUint16Seq (inclLower, exlUpper, length)  = this.rndUnsignRangeSeq inclLower exlUpper length 0us 1us

    ///random usigned 32-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndInt32Seq (inclLower, exlUpper, length)  = this.rndSignRangeSeq inclLower exlUpper length 0L 1L (int64) (int)

    ///random unsigned 32-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndUint32Seq (inclLower, exlUpper, length)  = this.rndUnsignRangeSeq inclLower exlUpper length 0u 1u

    ///random usigned 64-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndInt64Seq ((inclLower : int64), (exlUpper : int64), length)  = this.rndSignRangeSeq inclLower exlUpper length 0I 1I (this.bigint) (int64)

    ///random unsigned 64-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndUint64Seq (inclLower, exlUpper, length)  = this.rndUnsignRangeSeq inclLower exlUpper length 0UL 1UL

    (* unique random numbers in range sequences *)

    ///random unique usigned 8-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndSByteUniqueSeq (inclLower, exlUpper, length)  = this.rndSignUniqueSeq inclLower exlUpper length 0s 1s (int16) (sbyte)

    ///random unique unsigned 8-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndByteUniqueSeq (inclLower, exlUpper, length)  = this.rndUnsignUniqueSeq inclLower exlUpper length 0uy 1uy

    ///random unique signed 16-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndInt16UniqueSeq (inclLower, exlUpper, length)  = this.rndSignUniqueSeq inclLower exlUpper length 0 1 (int32) (int16)

    ///random unique unsigned 16-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndUint16UniqueSeq (inclLower, exlUpper, length)  = this.rndUnsignUniqueSeq inclLower exlUpper length 0us 1us

    ///random unique signed 32-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndInt32UniqueSeq (inclLower, exlUpper, length)  = this.rndSignUniqueSeq inclLower exlUpper length 0L 1L (int64) (int)

    ///random unique unsigned 32-bit integer in range inclusive of lower and exclusive of upper seq of length
    member this.RndUint32UniqueSeq (inclLower, exlUpper, length)  = this.rndUnsignUniqueSeq inclLower exlUpper length 0u 1u
    
        

    with
    interface IDisposable with
        member this.Dispose() = ()

