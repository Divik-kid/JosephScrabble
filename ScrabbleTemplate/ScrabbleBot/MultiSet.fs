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
    let remove a (n:uint32) = function
    |Xs s -> if s.TryFind(a).Value > n then Xs (s.Add(a, (numItems a (Xs s)-n))) else Xs (s.Remove(a))
    
    // 'a -> MultiSet<'a> -> MultiSet<'a>
    let removeSingle a  = function
    |Xs s -> if s.ContainsKey(a) && s.TryFind(a).Value >= 1u then Xs (s.Add(a, (numItems a (Xs s)-1u))) else Xs (s.Remove(a))
    //('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a
    let rec fold f acc = function
    |Xs s -> Map.fold f acc s 

    let foldBack f (Xs s) acc =  Map.foldBack f s acc