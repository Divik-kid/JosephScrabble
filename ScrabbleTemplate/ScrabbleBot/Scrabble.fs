namespace JosephScrabble

open System
open System.Collections.Generic
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad
open Types

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
    let myPoints = ref 0 



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
    
    let playGame cstream (pieces: Map<uint32, tile>) (timeout: uint32 option) (st: State.state) =
        let findWord (st: State.state) (starts: (coord * coord) list) =
            let mutable bestWord : int * (((coord * (uint32 * (char * int))) list * MultiSet.MultiSet<uint32>) * word) = (0, (([], MultiSet.empty), []))
            let rec aux (dict: Dictionary.Dict)
                        (pos: coord)
                        (start: coord)
                        (dir: coord)
                        (st: State.state)
                        (moves: (coord * (uint32 * (char * int))) list)
                        (isWord: bool)
                        (word: word)
                        (usedHand: MultiSet.MultiSet<uint32>)
                        (offset: int)
                        (score: (int * square) list) =
                let getScore (w: word) (score: (int * square) list) : int =
                    score |> List.map fst |> List.min |> (fun minOffset ->
                        score |> List.map (fun (i, s) -> (i-minOffset, s)))
                    |> List.map (fun (i, s) ->
                        s |> Map.toList
                        |> List.fold (fun acc (prio, sq) ->
                            (prio, sq w i)::acc) [])
                    |> List.fold (@) []
                    |> List.sortBy fst
                    |> List.map snd
                    |> List.fold (fun acc f ->
                        match f acc with
                        | Success i -> i
                        | Failure e -> failwith "Failed calculating points"(*This should never happen*)) 0
                    
                let incOffset i = i + if i <= 0 then -1 else 1
                    
                match getLetter pos st.playedLetters with
                | Some(c, p) ->
                    match Dictionary.step c dict with
                    | Some(b, newDict) ->
                        let (newScore: (int*square) list) =
                            (offset, (Map.add Int32.MinValue (fun _ _ acc -> acc + p |> Success) Map.empty))::score
                        aux newDict (pos ..+.. dir) start dir st moves b (word@[(c, p)]) usedHand (incOffset offset) newScore
                    | None -> None
                    |> ignore
                | None ->
                    // if is valid move
                    if isWord && List.length moves > 0 then
                        let curScore = getScore word score
                        // if better scoring than current best move
                        if curScore > (bestWord |> fst) then
                            lock bestWord (fun () ->
                                // double check that move is better, now that bestWord is locked
                                if curScore > (bestWord |> fst) then
                                    bestWord <- (curScore, ((moves, usedHand), word)))
                            
                    match Dictionary.reverse dict with
                    | Some(b, newDict) ->
                        aux newDict (start ..+.. (dir |> invCoord)) start (dir |> invCoord) st moves b (word |> List.rev) usedHand 1 score
                    | None -> None
                    |> ignore
                    
                    match st.board.squares pos with
                    | Success squareOpt ->
                        match squareOpt with
                        | Some sqr ->
                            (State.hand st) |> MultiSet.fold (fun acc letterId _ -> letterId::acc) []
                            |> List.fold (fun _ lId ->
                                let tile = Map.find lId pieces
                                
                                let newHand = (State.hand st) |> MultiSet.removeSingle lId
                                tile |> Set.fold (fun _ (c, p) ->
                                    match Dictionary.step c dict with
                                    | None -> None
                                    | Some (b, newDict) ->
                                        let newWord = word@[(c, p)]
                                        let newScore = (offset, sqr)::score
                                        let newSt = { st with hand = newHand }
                                        let newMoves = (pos, (lId, (c, p)))::moves
                                        
                                        // Find and check validity of parallel word and add parallel word score to newScore
                                        let rec getSideWord (dict: Dictionary.Dict) (pos: coord) (start: coord) (dir: coord) (isWord: bool) (word: word) (offset: int) (revved: bool) (score: (int * square) list) =
                                            match getLetter pos st.playedLetters with
                                            | Some (c, p) ->
                                                match Dictionary.step c dict with
                                                | Some (b, d) -> getSideWord d (pos ..+.. dir) start dir b (word@[(c, p)]) (offset |> incOffset) revved ((offset, Map.add Int32.MinValue (fun _ _ acc -> (acc + p) |> Success) Map.empty)::score)
                                                | None -> (false, word@[(c, p)]), score
                                            | None ->
                                                match (revved, Dictionary.reverse dict) with
                                                | _, Some (b, d) ->
                                                    getSideWord d (start ..+.. (dir |> invCoord)) start (dir |> invCoord) b (word |> List.rev) 1 true score
                                                | true,  None ->
                                                    (isWord, word), score
                                                | false, None ->
                                                    (isWord && (noLetter (start ..+.. (dir |> invCoord)) st.playedLetters), (word |> List.rev)), score
                                        
                                        let sideDir =
                                            match dir with
                                            | x, y when x + y < 0 -> y, x
                                            | x, y -> -y, -x
                                        let mutable res = (true, []), []
                                        match Dictionary.step c st.dict with
                                        | Some (_, d) ->
                                            res <- getSideWord d (pos ..+.. sideDir) pos sideDir false [c, p] 0 false [(0, sqr)]
                                        | None -> None |> ignore
                                        if res |> fst |> fst || res |> fst |> snd |> List.length <= 1 then
                                            let newScore =
                                                if res |> fst |> snd |> List.length > 1 then
                                                    let sideScore = getScore (res |> fst |> snd) (res |> snd)
                                                    (0, Map.add Int32.MaxValue (fun _ _ acc ->
                                                        (sideScore + acc) |> Success) Map.empty)::newScore
                                                else
                                                    newScore
                                        
                                            // hand-finishing bonus
                                            let newScore = if MultiSet.isEmpty newHand then
                                                                (offset, (Map.add Int32.MaxValue (fun _ _ acc -> (acc + 50) |> Success) Map.empty))::newScore
                                                            else
                                                                newScore
                                                                
                                            // Move to next square
                                            aux newDict (pos ..+.. dir) start dir newSt newMoves b newWord (usedHand |> MultiSet.addSingle lId) (offset |> incOffset) newScore
                                            |> ignore
                                        None) None
                                |> ignore
                                
                                None) None
                            |> ignore
                        | None -> None |> ignore // Edge of board
                        |> ignore
                    | Failure _ -> failwith "failed to find square on board" // Should never happen
                    |> ignore
                None
            
            let startWord (start: coord, dir: coord) =
                aux st.dict start start (dir |> invCoord) st [] false [] MultiSet.empty 0 [] |> ignore
            
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
            (*forcePrint ("Word: "   + (bestWord |> snd |> snd
                                      |> List.map fst
                                      |> String.Concat) + "\n")
            forcePrint ("Move: "   + (bestWord |> snd |> fst |> fst |> string) + "\n")
            forcePrint ("Points: " + (bestWord |> fst               |> string) + "\n")*)
            match bestWord with
            | n, _ when n < minScore -> None
            | _, m                   -> Some (m |> fst)
            
        
        let move (st: State.state) : ((coord * (uint32 * (char * int))) list * MultiSet.MultiSet<uint32>) option =
            let surroundCoord ((x, y): coord) : coord list =
                [(x+1, y);(x-1, y);(x, y+1);(x, y-1)]
            //Print.printHand pieces (State.hand st)
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
            let mutable usedTiles = MultiSet.empty
            if st.playerTurn = st.playerNumber then
                match move st with
                | Some (m, t) ->
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    usedTiles <- t
                    send cstream (SMPlay m)
                | None   ->
                    send cstream (SMChange (st.hand |> MultiSet.toList))

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let myPoints = List.fold (fun acc (c, (pid, (ch, i))) -> i + acc) 0 ms
                let playTiles = List.fold (fun acc (c, (pid, (ch, i))) -> c::acc) st.playedTiles ms
                let st' = st // This state needs to be updated
                
                let rec playLett (m: list<coord * (uint32 * (char * int))>) =
                    match m with 
                    |[] -> st'.playedLetters
                    |x::xs -> (addLetter (fst x) (snd (snd x))) (playLett xs)
                
                let newHand = MultiSet.subtract (State.hand st) usedTiles |> MultiSet.sum (newPieces |> MultiSet.ofList)

                debugPrint (sprintf "LETTT: %A\n" st'.playedLetters)
                debugPrint (sprintf "Played Tiles: %A\n" myPoints)
                debugPrint (sprintf "HAAAND: %A\n" st.hand)
                aux {st' with playerTurn = (st.playerTurn % st.numPlayers) + 1u; playedTiles = playTiles ; playedLetters = playLett ms; hand = newHand}  // This state needs to be updated
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)  
                let st' = st // This state needs to be updated
                //plays the tiles of the other player

                let playTile = List.fold (fun acc (c: coord, (pid, (ch, i))) -> c::acc) st.playedTiles ms
                let rec playLett (m: list<coord * (uint32 * (char * int))>) =
                    match m with 
                    |[] -> st'.playedLetters
                    |x::xs -> (addLetter (fst x) (snd (snd x))) (playLett xs)

                
                //use the pid:uint32 to search the pieces map for a value and add it to the played letters
               // let playLett = List.fold (Map.find pid pieces) st.playedLetters ms
                debugPrint (sprintf "LETTT2: %A\n" st'.playedLetters)
                debugPrint (sprintf "Played Tiles: %A\n" st'.playedTiles)
              
                aux {st' with playerTurn = (st.playerTurn % st.numPlayers) + 1u; playedTiles = playTile; playedLetters = playLett ms}             
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux {st' with playerTurn = (st.playerTurn % st.numPlayers)  + 1u}
            | RCM (CMChangeSuccess newPieces) ->
                aux {st with playerTurn = (st.playerTurn % st.numPlayers)  + 1u; hand = MultiSet.ofList newPieces}
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
        