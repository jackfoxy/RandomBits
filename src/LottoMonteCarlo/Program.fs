namespace RandomBits

module LottoMonteCarlo =

    open System
    open System.Collections.Generic

    [<EntryPoint>]
    let main argv = 
        Console.WriteLine ("")

        if argv.Length  = 0 then Console.WriteLine ("Enter the number of games to simulate between 0 and 2147483647 and two distinct integers between 1 and 100")
        else

            let x = new RandomBits()

            let scoreBoard = Dictionary()
            scoreBoard.Add ("winQuickPick", 0)
            scoreBoard.Add ("loseQuickPick", 0)
            scoreBoard.Add ("winMyNumbers", 0)
            scoreBoard.Add ("loseMyNumbers", 0)

            let myNum1 = SByte.Parse argv.[1]
            let myNum2 = SByte.Parse argv.[2]

            let rec loop dec =
                match dec with
                | 0 -> ()
                | _ -> 
                    let quickPick = List.ofSeq (x.RndSByteUniqueSeq (1y, 101y, 2))
                    let gameList = List.ofSeq (x.RndSByteUniqueSeq (1y, 101y, 2))

                    let game = Map.ofList (List.zip gameList gameList)

                    if (game.ContainsKey (Seq.item 0 quickPick)) && (game.ContainsKey (Seq.item 1 quickPick)) then    
                        let count = scoreBoard.["winQuickPick"]
                        scoreBoard.["winQuickPick"] <- count + 1          
                    else
                        let count = scoreBoard.["loseQuickPick"]
                        scoreBoard.["loseQuickPick"] <- count + 1  

                    if (game.ContainsKey myNum1) && (game.ContainsKey myNum2) then    
                        let count = scoreBoard.["winMyNumbers"]
                        scoreBoard.["winMyNumbers"] <- count + 1          
                    else
                        let count = scoreBoard.["loseMyNumbers"]
                        scoreBoard.["loseMyNumbers"] <- count + 1  

                    loop (dec - 1)

            loop (Int32.Parse argv.[0])

            printfn "win Quick Pick %s" (scoreBoard.["winQuickPick"].ToString("#,##0"))
            printfn "lose Quick Pick %s" (scoreBoard.["loseQuickPick"].ToString("#,##0"))

            printfn ""

            printfn "win My Numbers %s" (scoreBoard.["winMyNumbers"].ToString("#,##0"))
            printfn "lose My Numbers %s" (scoreBoard.["loseMyNumbers"].ToString("#,##0"))
         
        Console.WriteLine ("")
                                          
        //printfn "Hit any key to exit."
        //System.Console.ReadKey() |> ignore
        0
