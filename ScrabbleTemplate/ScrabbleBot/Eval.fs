module internal Eval

    open StateMonad
    open Types

    (* Code for testing *)

    let (hello: word) = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    // 6.7
    
    let binop f a b =
        a >>= (fun x -> b >>= (fun y -> ret (f x y)))
    
    let add = binop (+)
        
    // 6.8
        
    let div a b =
        a >>= (fun x -> b >>= (fun y -> if y <> 0 then ret (x/y) else fail DivisionByZero))
    
    // 6.9 helper functions
    
    let sub = binop (-)
    let mul = binop (*)

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    // 6.9
    
    let rec arithEval a : SM<int> =
        match a with
        | N n          -> ret n
        | V s          -> lookup s
        | WL           -> wordLength
        | PV a         -> arithEval a >>= pointValue
        | Add (a1, a2) -> add (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> sub (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> mul (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1, a2) ->
            (arithEval a1) >>= (fun x -> (arithEval a2) >>= (fun y -> if y <> 0 then ret (x%y) else fail DivisionByZero))
        | CharToInt c  -> charEval c >>= (int >> ret)
    and charEval c : SM<char> =
        match c with
        | C c         -> ret c
        | CV a        -> (arithEval a) >>= characterValue
        | ToUpper c   -> (charEval c)  >>= (System.Char.ToUpper >> ret)
        | ToLower c   -> (charEval c)  >>= (System.Char.ToLower >> ret)
        | IntToChar a -> (arithEval a) >>= (char >> ret)
    and boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false

        | AEq (a1, a2) -> (arithEval a1) >>= (fun x -> (arithEval a2) >>= (fun y -> ret (x=y)))
        | ALt (a1, a2) -> (arithEval a1) >>= (fun x -> (arithEval a2) >>= (fun y -> ret (x<y)))

        | Not b         -> (boolEval b)  >>= (fun x -> ret (not x))
        | Conj (b1, b2) -> (boolEval b1) >>= (fun x -> (boolEval b2) >>= (fun y -> ret (x&&y))) 

        | IsVowel c  -> (charEval (ToUpper c)) >>= (fun c -> List.contains c ['A'; 'E'; 'I'; 'O'; 'U'] |> ret)
        | IsLetter c -> (charEval c) >>= (System.Char.IsLetter >> ret)
        | IsDigit c  -> (charEval c) >>= (System.Char.IsDigit  >> ret)


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare s          -> declare s
        | Ass (s, aexp)      -> (arithEval aexp) >>= (update s)
        | Skip               -> ret ()
        | Seq (s1, s2)       -> (stmntEval s1) >>>= (stmntEval s2)
        | ITE (bexp, s1, s2) -> (boolEval bexp) >>= (fun b -> stmntEval (if b then s1 else s2))
        | While (bexp, s)    -> (boolEval bexp) >>= (fun b -> stmntEval (if b then Seq(s, stmnt) else Skip))

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *)
    
    // 6.12

    type squareFun = word -> int -> int -> Result<int, Error>
    let stmntToSquareFun stm = fun w pos acc ->
        (stmntEval stm) >>>= (lookup "_result_")
        |> evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"])
    
    // 6.13

    type squareStmnt = Map<int, stmnt>
    
    let stmntsToSquare (stms : squareStmnt) : Map<int, squareFun> = stms |> Map.map (fun _ -> stmntToSquareFun)
    
    // 6.14
    
    let stmntToBoardFun stm m = fun (x, y) ->
        (stmntEval stm) >>>= (lookup "_result_")
        |> evalSM (mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"])
        |> function
            | Success i ->
                match m |> Map.tryFind i with
                | Some stm -> Success (Some stm)
                | None     -> Success None
            | Failure err -> Failure err

    // 6.15
    
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let mkBoard (c: coord) (defaultSq: int) (boardStmnt: stmnt) (ids: Map<int, squareStmnt>) : board = {
        center        = c
        defaultSquare = ids |> Map.find defaultSq |> stmntsToSquare
        squares       = ids |> Map.map (fun _ -> stmntsToSquare) |> stmntToBoardFun boardStmnt
    }
    