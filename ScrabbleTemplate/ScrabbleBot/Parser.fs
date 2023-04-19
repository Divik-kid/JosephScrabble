module internal ImpParser

    open Eval
    open Types
    open ScrabbleUtil

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    // 7.1
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    // 7.2
    
    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    // 7.3
    
    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    // 7.4
    
    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' <?> "parenthesise"
    
    let brace p = pchar '{' >*>. p .>*> pchar '}' <?> "brace"
    
    // 7.5

    let pid = (pletter <|> satisfy ((=) '_')) .>>. many (palphanumeric <|> satisfy ((=) '_'))
              |>> (fun (c, cs) -> c::cs |> System.String.Concat)

    // 7.6
    
    let unop op p = op >*>. p
    
    // 7.7
    
    let binop op p1 p2 = p1 .>*> op .>*>. p2 // incorrect (not implemented)
    
    // 7.8

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    let CParse, cref = createParserForwardedToRef<cExp>()
    
    let LogiParse, lref  = createParserForwardedToRef<bExp>()
    let CompParse, coref = createParserForwardedToRef<bExp>()
    let UniBParse, uref  = createParserForwardedToRef<bExp>()
    
    let StmntBaseParse, baseref  = createParserForwardedToRef<stmnt>()
    let StmntParse, stmntref  = createParserForwardedToRef<stmnt>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') ProdParse |>> (fun a -> Mul (N -1, a)) <?> "Neg"
    let PVParse  = pPointValue >*>. AtomParse |>> PV <?> "PV"
    let VarParse = pid |>> V <?> "Var"
    let CToIParse = pCharToInt >*>. parenthesise CParse |>> CharToInt <?> "CharToInt"
    do aref := choice [NegParse; NParse; PVParse; CToIParse; VarParse; ParParse]

    let AexpParse = TermParse 

    // 7.9
    
    let CharParse = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "Char"
    let CVParse   = pCharValue >*>. ParParse |>> CV <?> "CV"
    let IToCParse = pIntToChar >*>. ParParse |>> IntToChar <?> "IntToChar"
    let UppParse  = pToUpper >*>. parenthesise CParse |>> ToUpper <?> "ToUpper"
    let LowParse  = pToLower >*>. parenthesise CParse |>> ToLower <?> "ToLower"
    do cref := choice [CharParse; CVParse; UppParse; LowParse; IToCParse]
    
    let CexpParse = CParse
    
    // 7.10
    
    let AndParse = CompParse .>*> pstring "/\\" .>*>. LogiParse |>> Conj <?> "And"
    let OrParse  = CompParse .>*> pstring "\\/" .>*>. LogiParse |>> (fun (a, b) -> Not (Conj (Not a, Not b))) <?> "Or"
    do lref := choice [AndParse; OrParse; CompParse]
    
    let EqParse  = AexpParse .>*> pchar '=' .>*>. AexpParse |>> AEq <?> "Eq"
    let NeqParse = AexpParse .>*> pstring "<>" .>*>. AexpParse |>> (fun (a, b) -> Not (AEq (a, b))) <?> "Neq"
    let LtParse  = AexpParse .>*> pchar '<' .>*>. AexpParse |>> ALt <?> "Lt"
    let LteParse = AexpParse .>*> pstring "<=" .>*>. AexpParse |>> (fun (a, b) -> Not (Conj (Not (ALt (a, b)), Not (AEq (a, b))))) <?> "Lte"
    let GtParse  = AexpParse .>*> pchar '>' .>*>. AexpParse |>> (fun (a, b) -> Conj (Not (AEq (a, b)), Not (ALt (a, b)))) <?> "Gt"
    let GteParse = AexpParse .>*> pstring ">=" .>*>. AexpParse |>> (fun (a, b) -> Not (ALt (a, b))) <?> "Gte"
    do coref := choice [EqParse; NeqParse; LtParse; LteParse; GtParse; GteParse; UniBParse]
    
    let NotParse      = pchar '~' >*>. LogiParse |>> Not <?> "Not"
    let IsDigitParse  = pIsDigit >*>. parenthesise CexpParse |>> IsDigit <?> "IsDigit"
    let IsLetterParse = pIsLetter >*>. parenthesise CexpParse |>> IsLetter <?> "IsLetter"
    let IsVowelParse  = pIsVowel >*>. parenthesise CexpParse |>> IsVowel <?> "IsVowel"
    let ParBoolParse  = parenthesise LogiParse
    let TrueParse     = pTrue |>> (fun _ -> TT) <?> "True"
    let FalseParse    = pFalse |>> (fun _ -> FF) <?> "False"
    do uref := choice [NotParse; IsDigitParse; IsLetterParse; IsVowelParse; ParBoolParse; TrueParse; FalseParse]

    let BexpParse = LogiParse

    // 7.11
    
    let SeqParse = StmntParse .>*> pchar ';' .>*>. StmntBaseParse |>> Seq <?> "Seq"
    do baseref := choice [SeqParse; StmntParse]
    
    let AssParse        = pid .>*> pstring ":=" .>*>. AexpParse |>> Ass <?> "Ass"
    let DeclareParse    = pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"
    let IfThenElseParse = pif >*>. ParBoolParse .>*>.
                          (pthen >*>. brace StmntBaseParse .>*> pelse .>*>. brace StmntParse)
                          |>> (fun (b, (s1, s2)) -> ITE (b, s1, s2)) <?> "IfThenElse"
    let IfThenParse     = pif >*>. ParBoolParse .>*>
                          pthen .>*>. brace StmntBaseParse
                          |>> (fun (b, s) -> ITE (b, s, Skip)) <?> "IfThen"
    let WhileParse      = pwhile >*>. ParBoolParse .>*> pdo .>*>. brace StmntBaseParse |>> While <?> "While"
    do stmntref := choice [AssParse; DeclareParse; IfThenElseParse; IfThenParse; WhileParse]
    
    let stmntParse = StmntBaseParse
    
    // 7.12

    let parseSquareProg = Map.map (fun _ -> run stmntParse >> getSuccess) >> stmntsToSquare
    
    // 7.13
    
    let parseBoardFun (s: string) (m: Map<int, square>) : boardFun =
        stmntToBoardFun (run stmntParse s |> getSuccess) m
    
    // 7.14

    let parseBoardProg (s: string) (m: Map<int, squareProg>) : boardProg = {
        prog       = s
        squares    = m
        usedSquare = 0
        center     = (0, 0)
    
        isInfinite = false
        ppSquare   = ""
    }

    let mkBoard (bp : boardProg) : board = {
        center = bp.center
        defaultSquare = bp.squares |> Map.find 0 |> parseSquareProg
        squares = bp.squares |> Map.map (fun _ -> parseSquareProg) |> parseBoardFun bp.prog
    }

