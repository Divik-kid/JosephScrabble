module internal MultiSet

    type MultiSet<'a when 'a : comparison> = Xs of Map<'a, uint32>

    let empty = Xs Map.empty 

    let isEmpty (Xs s) = s.IsEmpty

    let size (Xs s) = Map.fold (fun acc _ item -> acc + item) 0u s

    let contains a (Xs s) = s.ContainsKey(a)
    // 'a -> MultiSet<'a> -> uint32

    let numItems a = function 
    |Xs b -> if b.ContainsKey(a) then b.TryFind(a).Value else 0u

    //'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    let rec add a (n:uint32) =  function 
    |Xs s -> Xs (s.Add(a, (numItems a (Xs s)+n)))

    //'a -> MultiSet<'a> -> MultiSet<'a>
    let addSingle a = function
    |Xs s -> Xs (s.Add(a, (numItems a (Xs s)+1u)))

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