module internal MultiSet

    type MultiSet<'a when 'a : comparison> = Xs of Map<'a, uint32>

    let empty = Xs Map.empty 

    let isEmpty (Xs s) = s.IsEmpty

    let size (Xs s) = Map.fold (fun acc _ item -> acc + item) 0u s

    let contains a (Xs s) = s.ContainsKey(a)
    // 'a -> MultiSet<'a> -> uint32

    let numItems a (Xs m) =  
        match Map.tryFind a m with
        | Some v -> v
        | None   -> 0u

    //'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    let rec add a (n:uint32) =  function 
    |Xs s -> Xs (s.Add(a, (numItems a (Xs s)+n)))

    //'a -> MultiSet<'a> -> MultiSet<'a>
    let addSingle a ms = add a 1u ms

    //'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    let remove e n = function
        | Xs m ->
            let num = numItems e (Xs m)
            if num <= n then
                Xs (m.Remove e)
            else
                Xs (m.Add (e, num - n))
    
    // 'a -> MultiSet<'a> -> MultiSet<'a>
    let removeSingle a ms = remove a 1u ms
    //('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a
    let rec fold f acc = function
    |Xs s -> Map.fold f acc s 

    let foldBack f (Xs s) acc =  Map.foldBack f s acc
    
    let toList (Xs s) =
        Map.fold (fun state k v ->
            let rec aux acc = function
                | 0u -> acc
                | n  -> aux (k::acc) (n-1u)
            aux state v) [] s
    
    let ofList l =
        let rec aux acc = function
        | []    -> acc
        | (a, b)::xs -> aux (acc |> add a b) xs 
        
        aux empty l
        
    let map (f: 'a -> 'b) ms = fold (fun s k v -> add (f k) v s) empty ms
    let union (ms1: MultiSet<'a>) (ms2: MultiSet<'a>) =
            fold (fun s k v ->
                if numItems k s < v then
                    add k (v - numItems k s) s
                else
                    s
            ) ms1 ms2
    
    let sum (ms1: MultiSet<'a>) (ms2: MultiSet<'a>) = fold (fun s k v -> add k v s) ms1 ms2
    let subtract (ms1: MultiSet<'a>) (ms2: MultiSet<'a>) = fold (fun s k v -> remove k v s) ms1 ms2
    let intersection (ms1: MultiSet<'a>) (ms2: MultiSet<'a>) = 
            fold (fun s k v ->
                if (numItems k ms1) > 0u then
                    add k (min v (numItems k ms1)) s
                else
                    s
            ) empty ms2
