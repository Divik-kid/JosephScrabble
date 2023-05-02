﻿namespace JosephScrabble

open System
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Types.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        
        numPlayers    : uint32
        playerTurn    : uint32
        
        playedTiles   : coord list
        playedLetters : Map<int, Map<int, char*int>>
        
        startTime     : DateTime
    }

    let mkState b d pn h num turn curTime = {
        board = b
        dict = d
        playerNumber = pn
        hand = h
        numPlayers = num
        playerTurn = turn
        playedTiles = []
        playedLetters = Map.empty
        startTime = curTime;
    }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand


module Scrabble =
    open System.Threading
    open System.Threading.Tasks
    
    let minScore = 1
    
    let getLetter ((x, y): coord) (m: Map<int, Map<int, char*int>>) : (char*int) option =
        match m |> Map.tryFind x with
        | Some m2 -> m2 |> Map.tryFind y
        | None    -> None
    
    let noLetter (c: coord) = getLetter c >> (=) None
    let hasLetter c m = noLetter c m |> not
    
    let (..+..) ((x1, y1): coord) ((x2, y2): coord) : coord = (x1+x2, y1+y2)
    let (..*..) ((x, y): coord) (m: int) : coord = (x*m, y*m)
    let invCoord (c: coord) : coord = c ..*.. -1
    let rotateCord ((x, y): coord) : coord = (y, x)
    let sideCords c = [c |> rotateCord; c |> invCoord |> rotateCord]
    let addLetter ((x, y): coord) l (m : Map<int, Map<int, char*int>>) =
        match m |> Map.tryFind x with
        | Some m2 -> m |> Map.add x (m2 |> Map.add y l)
        | None    -> m |> Map.add x (Map.empty |> Map.add y l)
    
    let potentialStart (c: coord) m (dir: coord) : bool =
        match hasLetter c m with
        | true  -> noLetter (c ..+.. dir) m
        | false -> noLetter (c ..+.. dir) m && noLetter (c ..+.. (invCoord dir)) m
    
    let playGame cstream pieces (timeout: uint32 option) (st: State.state) =
        let findWord (st: State.state) (starts: (coord * coord) list) =
            let mutable bestWord : int * (coord * (uint32 * (char * int))) list = (0, [])
            let rec aux (dict: Dictionary.Dict)
                        (pos: coord)
                        (start: coord)
                        (dir: coord)
                        (st: State.state)
                        (moves: (coord * (uint32 * (char * int))) list)
                        (isWord: bool)
                        (score: (int * (int -> int)) list) =
                match getLetter pos st.playedLetters with
                | Some(c, p) ->
                    match Dictionary.step c dict with
                    | Some(b, newDict) -> aux newDict (pos ..+.. dir) start dir st moves b (((0, (fun acc -> p + acc))::score) |> List.sortBy fst)
                    | None -> None
                    |> ignore
                | None ->
                    // if is valid move
                    if isWord && List.length moves > 0 then
                        let curScore = (score |> List.fold (fun acc (_, f) -> f acc) 0)
                        // if better scoring than current best move
                        if curScore > (bestWord |> fst) then
                            lock bestWord (fun () ->
                                // double check that move is better, now that bestWord is locked
                                if curScore > (bestWord |> fst) then
                                    bestWord <- (curScore, moves))
                            
                    match Dictionary.reverse dict with
                    | Some(b, newDict) -> aux newDict (start ..+.. (dir |> invCoord)) start (dir |> invCoord) st moves b score
                    | None -> None
                    |> ignore
                    
                    match st.board.squares pos with
                    | Success squareOpt ->
                        match squareOpt with
                        | Some sqr -> None // TODO: do word
                        | None -> None // Edge of board
                        |> ignore
                    | Failure e -> failwith "failed to find square on board" // Should never happen
                None
            
            let startWord (start: coord, dir: coord) =
                aux st.dict start start (dir |> invCoord) st [] false [] |> ignore
            
            use cts = match timeout with
                      | Some t -> new CancellationTokenSource((t |> float) * 0.98 |> int)
                      | None   -> new CancellationTokenSource()
            let po = ParallelOptions()
            po.CancellationToken <- cts.Token
            po.MaxDegreeOfParallelism <- Environment.ProcessorCount
            try
                Parallel.ForEach (starts, po, startWord) |> ignore
            with
            | :? OperationCanceledException -> printfn "Timeout"
            
            match bestWord with
            | n, _ when n < minScore -> None
            | _, m                   -> Some m
            
        
        let move (st: State.state) : (coord * (uint32 * (char * int))) list option =
            let surroundCoord ((x, y): coord) : coord list =
                [(x+1, y);(x-1, y);(x, y+1);(x, y-1)]
            Print.printHand pieces (State.hand st)
            let wordStarts = List.fold (fun s -> surroundCoord >> (@) s) st.playedTiles st.playedTiles
                             |> List.distinct
                             |> (fun l -> if List.length st.playedTiles = 0 then [st.board.center] else l)
                             |> List.fold (fun acc c ->
                                 [(1, 0); (0, 1)]
                                 |> List.fold (fun acc2 dir ->
                                     match potentialStart c st.playedLetters dir with
                                     | true  -> (c, dir)::acc2
                                     | false -> acc2) acc) []
                             
            findWord st wordStarts
            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            //let input =  System.Console.ReadLine()
            //RegEx.parseMove input |> Some

        let rec aux (st : State.state) =
            if st.playerTurn = st.playerNumber then
                match move st with
                | Some m ->
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay m)
                | None   ->
                    send cstream (SMChange (st.hand |> MultiSet.toList))

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = st // This state needs to be updated
                aux {st' with playerTurn = (st.playerTurn + 1u) % st.numPlayers}
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux {st' with playerTurn = (st.playerTurn + 1u) % st.numPlayers}
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux {st' with playerTurn = (st.playerTurn + 1u) % st.numPlayers}
            | RCM (CMChangeSuccess newPieces) ->
                aux {st with playerTurn = (st.playerTurn + 1u) % st.numPlayers; hand = MultiSet.ofList newPieces}
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg)
            (dictf : bool -> Dictionary.Dict)
            (numPlayers : uint32)
            (playerNumber : uint32)
            (playerTurn  : uint32)
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option)
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles timeout (State.mkState board dict playerNumber handSet numPlayers playerTurn DateTime.Now)
        