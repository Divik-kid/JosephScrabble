module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero
        | ReservedName of string

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved =
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') ->
                match f b with
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> =
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    // Green exercises
    // 6.1

    let pop : SM<unit> =
        S (fun s ->
            match s.vars with
            | []   -> failwith "Popped empty stack."
            | v::_ -> Success ((), {s with vars = s.vars.Tail}))

    // 6.2
    
    let wordLength : SM<int> =
        S (fun s ->
            Success (s.word |> List.length, s))
    
    // 6.3

    let characterValue (pos : int) : SM<char> =
        S (fun s ->
            if 0 <= pos && pos < s.word.Length then
                Success (s.word.[pos] |> fst, s)
            else
                Failure (IndexOutOfBounds pos))

    // 6.4
    
    let pointValue (pos : int) : SM<int> =
        S (fun s ->
            if 0 <= pos && pos < s.word.Length then
                Success (s.word.[pos] |> snd, s)
            else
                Failure (IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> =
        let rec aux =
            function
            | []      -> None
            | m :: ms ->
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s ->
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))
        
    // Yellow exercises
    // 6.5

    let update (var : string) (value : int) : SM<unit> =
        let rec aux vars acc =
            match vars with
            | [] -> None
            | m::ms ->
                match Map.tryFind var m with
                | Some _ -> Some (acc @ ((Map.add var value m)::ms))
                | None   -> aux ms (acc @ [m])
            
        S (fun s ->
            match aux s.vars [] with
            | Some ms -> Success ((), {s with vars = ms})
            | None   -> Failure (VarNotFound var))
    
    // 6.6
    
    let declare (var : string) : SM<unit> =
        S (fun s ->
            if Set.contains var s.reserved then
                Failure (ReservedName var)
            else
                match s.vars with
                | m::ms ->
                    match Map.tryFind var m with
                    | Some _ -> Failure (VarExists var)
                    | None   -> Success ((), {s with vars = (Map.add var 0 m)::ms})
                | _ -> failwith "Attempted to declare a variable with no environment.")
