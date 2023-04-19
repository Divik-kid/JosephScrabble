module internal Dictionary

    open Microsoft.FSharp.Collections

    type Dict =
        D of Map<char, bool * Dict>
        
    let revChar = char 0
    
    let empty () = D Map.empty
    let insert (word: string) (dict: Dict) =
        let permutations (word: string) =
            List.fold (fun ((last, st): char list * char list list) (c: char) ->
                    ([c]@last, (List.map (fun e -> (@) e [c]) st)@[[c]@last])
                    ) ([revChar], []) (Seq.toList word) |> snd
        
        let rec recInsert dict = function
            | [] -> failwith "Should never happen"
            | [c] ->
                match dict with
                    | D m ->
                        let o = m.TryFind c
                        D (Map.add c (true, if o.IsSome then (o.Value |> snd) else empty()) m)
            | c::cs ->
                match dict with
                    | D m ->
                        let o = m.TryFind c
                        if o.IsSome then
                            D (Map.add c (o.Value |> fst, recInsert (o.Value |> snd) cs) m)
                        else
                            D (Map.add c (false, recInsert (empty()) cs) m)
        List.fold recInsert dict (permutations word)
    
    let step c = function
        | D m -> m.TryFind c
    
    let reverse = function
        | D m -> m.TryFind revChar
    
    let rec lookup (word: string) dict =
        Seq.toList word
        |> List.rev
        |> List.fold (fun (b, d) c ->
            if not b then
                (false, empty())
            else
                let o = step c d
                if o.IsSome then
                    (true, o.Value |> snd)
                else
                    (false, empty())
        ) (true, dict)
        |> snd
        |> reverse
        |> fun o -> if o.IsSome then (o.Value |> fst) else false