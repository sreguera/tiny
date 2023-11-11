module Ilvm exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Word exposing (Word)


type alias Address = Int


type Opcode 
    = TST Address String   -- Check if current buf starts by word or else jump to address.
    | CALL Address         -- Execute the IL subroutine at address. Save next inst in control stack.
    | RTN                  -- Return from IL subroutine to the inst at the top of the control stack.
    | DONE                 -- Report a syntax error if the rest of the line is not empty.
    | JMP Address          -- Continue execution at the address.
    | PRS                  -- Print characters up to the quote and move the cursor past it.
    | PRN                  -- Print number at top of the stack.
    | SPC                  -- Insert spaces to move print head to next zone.
    | NLINE                -- Output CRLF to printer.
    | NXT                  -- Go to next line unless in direct execution.
    | RUNXT                -- Start or continue program execution.
    | XFER                 -- Go to line at top of astack or error if it doesn't exist.
    | SAV                  -- Push current line number onto sbrstk.
    | RSTR                 -- Replace current line number with top of sbrstk.
    | CMPR                 -- Compare top and top-2 as indicated by top-1, and NXT if false.
    | LIT Int              -- Push the number onto the aestk.
    | INNUM                -- Read a number from the terminal and put it in aestk.  
    | FIN                  -- Return to the line collect routine.
    | ERR                  -- Report syntax error and return to line collect routine.
    | ADD                  -- Replace top two elements of aestk by their sum.
    | SUB                  -- Replace top two elements of aestk by their difference.
    | NEG                  -- Replace top of aestk with its negative.
    | MUL                  -- Replace top two elements of aestk by their product.
    | DIV                  -- Replace top two elements of aestk by their quotient.
    | STORE                -- Place the value at top of aestk at the var below it.
    | TSTV Address         -- Test for variable and put its index on stack, or continue at address.
    | TSTN Address         -- Test for number and put it on stack, or continue at address.
    | IND                  -- Replace top of aestk by the var it indexes.
    | LST                  -- List the contents of the program area.
    | INIT                 -- Global initialization.
    | GETLINE              -- Input a line to lbuf.
    | TSTL Address         -- Check if current buf starts by number or else jump to address.
    | INSRT                -- Insert current line buf in the line store.
    | XINIT                -- Initialization for execution.


type alias VM =
    { pc : Address
    , code : Array Opcode
    , lbuf : String
    , aestk : List Word
    , cstk : List Address
    , vars : Dict Int Word
    , curline : Int
    , sbrstk : List Int
    , lines : Dict Int String
    , output : String
    , next : Next
    }


type Next
    = Continue
    | Stop
    | Input (VM -> VM)


makeVM : Array Opcode -> VM
makeVM opcodes =
    { pc = 0
    , code = opcodes
    , lbuf = ""
    , aestk = []
    , cstk = []
    , vars = Dict.empty
    , curline = 0
    , sbrstk = []
    , lines = Dict.empty
    , output = ""
    , next = Continue
    }


makeErrorVM : String -> VM
makeErrorVM err =
    let
        errorCodes = Array.fromList [ GETLINE, JMP 0 ]
        errorVM = makeVM errorCodes
    in
    { errorVM | output = err }


span : (Char -> Bool) -> String -> (String, String)
span pred s =
    case String.uncons s of
        Just (h, t) ->
            if pred h then
                let 
                    (take, drop) = span pred t
                in
                (String.cons h take, drop)
            else
                ("", s)
        Nothing ->
            ("", s)


read_addr : Address
read_addr = 2


stmt_addr : Address
stmt_addr = 7


error0 : String -> VM -> VM
error0 msg vm =
    let
        line = String.concat ["\n! ", String.fromInt vm.curline, ": ", msg, "\n"]
    in
    { vm | pc = read_addr, curline = 0, output = String.append vm.output line }


parseNum : VM -> VM
parseNum vm =
    case String.toInt vm.lbuf of
        Just n ->
            { vm | lbuf = "", aestk = Word.fromInt n :: vm.aestk }
        Nothing ->
            error0 "Syntax error" vm


exec1 : VM -> VM
exec1 vm =
    let

        error : String -> VM
        error msg =
            let
                line = String.concat ["\n! ", String.fromInt vm.curline, ": ", msg, "\n"]
            in
            { vm | pc = read_addr, curline = 0, output = String.append vm.output line }

        sysError : String -> VM
        sysError msg =
            let
                line = String.concat ["\n!!! ", String.fromInt vm.curline, ": ", msg, "\n"]
            in
            { vm | output = String.append vm.output line, next = Stop }

        nxt : VM -> VM
        nxt vm0 =
            if vm0.curline == 0 then
                { vm0 | pc = read_addr }
            else
                runxt vm0
        
        runxt : VM -> VM
        runxt vm0 =
            let
                next = List.minimum <| List.filter (\x -> x > vm0.curline) <| Dict.keys vm0.lines
            in
            case next of
                Just lineno ->
                    { vm0 | pc = stmt_addr, curline = lineno, lbuf = Maybe.withDefault "" (Dict.get lineno vm0.lines) }
                _ ->
                    { vm0 | pc = read_addr, curline = 0, lbuf = "" }
    in
    case Maybe.withDefault ERR (Array.get vm.pc vm.code) of
        INIT ->
            { vm | pc = vm.pc + 1
                , lbuf = ""
                , aestk = []
                , cstk = []
                , vars = Dict.empty
                , curline = 0
                , sbrstk = []
                , lines = Dict.empty 
            }

        XINIT ->
            { vm | pc = vm.pc + 1, aestk = [], cstk = [] }

        NXT ->
            nxt vm

        RUNXT ->
            runxt vm

        XFER ->
            case vm.aestk of
                a :: rest ->
                    case Dict.get (Word.toInt a) vm.lines of
                        Just code ->
                            { vm | pc = stmt_addr, curline = (Word.toInt a), lbuf = code, aestk = rest }
                        _ ->
                            error "Missing line"
                _ ->
                    sysError "Stack underflow"

        DONE ->
            if String.isEmpty (String.trim vm.lbuf) then
                { vm | pc = vm.pc + 1 }
            else
                error "Syntax error"
        
        ERR ->
            error "Syntax error"

        GETLINE ->
            -- No change in pc, blocked in I/O
            { vm | next = Input identity }

        INNUM ->
            -- No change in pc, blocked in I/O
            { vm | next = Input parseNum }

        FIN ->
            { vm | pc = read_addr } -- XXX curline=0?

        JMP addr ->
            { vm | pc = addr }

        CALL addr ->
            { vm | pc = addr, cstk = vm.pc + 1 :: vm.cstk }

        RTN ->
            case vm.cstk of
                a :: rest ->
                    { vm | pc = a, cstk = rest }
                _ ->
                    sysError "Control stack underflow"

        SAV ->
            { vm | pc = vm.pc + 1, sbrstk = vm.curline :: vm.sbrstk }

        RSTR ->
            case vm.sbrstk of
                a :: rest ->
                    { vm | pc = vm.pc + 1, sbrstk = rest, curline = a }
                _ ->
                    error "RETURN without GOSUB"

        INSRT ->
            let
                (lnums, code) = span Char.isDigit vm.lbuf
            in
            case String.toInt lnums of
                Just lnum ->  -- TODO: Check >0 and <limit
                    case code of
                        "" ->
                            { vm | pc = vm.pc + 1, lbuf = "", lines = Dict.remove lnum vm.lines }
                        _ ->
                            { vm | pc = vm.pc + 1, lbuf = "", lines = Dict.insert lnum code vm.lines }
                Nothing ->
                    sysError "No line number"

        TSTL addr ->
            let
                lbuf = String.trimLeft vm.lbuf
            in
            case String.uncons lbuf of
                Just (n, _) ->
                    if Char.isDigit n then
                        { vm | lbuf = lbuf, pc = vm.pc + 1 }
                    else
                        { vm | lbuf = lbuf, pc = addr }
                Nothing ->
                    { vm | lbuf = lbuf, pc = addr }

        TST addr str ->
            let
                lbuf = String.trimLeft vm.lbuf
            in
            if String.startsWith str lbuf then
                { vm | lbuf = String.dropLeft (String.length str) lbuf, pc = vm.pc + 1 }
            else
                { vm | lbuf = lbuf, pc = addr }

        TSTV addr ->
            let
                lbuf = String.trimLeft vm.lbuf
            in
            case String.uncons lbuf of
                Just (v, _) ->
                    if Char.isUpper v then
                        { vm | lbuf = String.dropLeft 1 lbuf, aestk = Word.fromInt (Char.toCode v - Char.toCode 'A') :: vm.aestk, pc = vm.pc + 1 }
                    else
                        { vm | lbuf = lbuf, pc = addr }
                Nothing ->
                    { vm | lbuf = lbuf, pc = addr }

        TSTN addr ->
            let
                lbuf = String.trimLeft vm.lbuf
                (lnums, rest) = span Char.isDigit lbuf
            in
            case String.toInt lnums of
                Just lnum ->
                    { vm | pc = vm.pc + 1, lbuf = rest, aestk = Word.fromInt lnum :: vm.aestk }
                Nothing ->
                    { vm | pc = addr, lbuf = lbuf }

        PRS ->
            let
                (out, rest) = span (\c -> c /= '"') vm.lbuf
            in
            if String.startsWith "\"" rest then
                { vm | pc = vm.pc + 1, lbuf = String.dropLeft 1 rest, output = String.append vm.output out }
            else
                error "Syntax error"

        PRN ->
            case vm.aestk of
                a :: rest ->
                    { vm | pc = vm.pc + 1, aestk = rest, output = String.append vm.output (String.fromInt (Word.toInt a)) }
                _ ->
                    sysError "Stack underflow"

        SPC ->
            { vm | pc = vm.pc + 1, output = String.append vm.output " " }

        NLINE ->
            { vm | pc = vm.pc + 1, output = String.append vm.output "\n" }

        LST ->
            let
                lines = String.concat (List.map (\(k, v) -> String.fromInt k ++ " " ++ v ++ "\n") (Dict.toList vm.lines))
            in
            { vm | pc = vm.pc + 1, output = String.append vm.output lines }

        STORE ->
            case vm.aestk of
                a :: b :: rest ->
                    { vm | pc = vm.pc + 1, aestk = rest, vars = Dict.insert (Word.toInt b) a vm.vars } -- TODO: System Error, Invalid var
                _ ->
                    sysError "Stack underflow"

        IND ->
            case vm.aestk of
                a :: rest ->
                    let
                        val = Maybe.withDefault (Word.fromInt 0) (Dict.get (Word.toInt a) vm.vars)
                    in
                    { vm | pc = vm.pc + 1, aestk = val :: rest } -- TODO: System Error, Invalid var
                _ ->
                    sysError "Stack underflow"

        LIT val ->
            { vm | pc = vm.pc + 1, aestk = (Word.fromInt val) :: vm.aestk }

        ADD ->
            case vm.aestk of
                a :: b :: rest ->
                    { vm | pc = vm.pc + 1, aestk = Word.add a  b :: rest }
                _ ->
                    sysError "Stack underflow"

        SUB ->
            case vm.aestk of
                a :: b :: rest ->
                    { vm | pc = vm.pc + 1, aestk = Word.minus b  a :: rest }
                _ ->
                    sysError "Stack underflow"

        NEG ->
            case vm.aestk of
                a :: rest ->
                    { vm | pc = vm.pc + 1, aestk = Word.neg a :: rest }
                _ ->
                    sysError "Stack underflow"

        MUL ->
            case vm.aestk of
                a :: b :: rest ->
                    { vm | pc = vm.pc + 1, aestk = Word.mul a b :: rest }
                _ ->
                    sysError "Stack underflow"

        DIV ->
            case vm.aestk of
                a :: b :: rest ->
                    case Word.div b a of
                        Just n ->
                            { vm | pc = vm.pc + 1, aestk = n :: rest }
                        _ ->
                            error "Division by zero"
                _ ->
                    sysError "Stack underflow"

        CMPR ->
            case vm.aestk of
                r :: c :: l :: rest ->
                    let
                        res = case (Word.toInt c) of
                            0 -> -- ==
                                Just (Word.eq l r)
                            1 -> -- <
                                Just (Word.less l r)
                            2 -> -- <=
                                Just (not (Word.less r l))
                            3 -> -- /=
                                Just (not (Word.eq l r))
                            4 -> -- > 
                                Just (Word.less r l) 
                            5 -> -- >=
                                Just (not (Word.less l r)) 
                            _ ->
                                Nothing
                    in
                    case res of
                        Just val ->
                            if val then
                                { vm | pc = vm.pc + 1, aestk = rest }
                            else
                                nxt { vm | aestk = rest }
                        Nothing ->
                            sysError ("Invalid comparison " ++ String.fromInt (Word.toInt c))
                _ ->
                    sysError "Stack underflow"


execN : VM -> Int -> VM
execN vm n =
    if n == 0 then
        vm
    else
        let
            vm1 = exec1 vm
        in
        case vm1.next of
            Continue ->
                execN vm1 (n - 1)
            _ ->
                vm1


resume : VM -> VM
resume vm =
    execN vm 100


resumeWithInput : VM -> String -> VM
resumeWithInput vm s =
    resume <| 
        case vm.next of 
            Input complete ->
                let
                    vm1 = complete { vm | lbuf = s }
                in
                { vm1 | pc = vm1.pc + 1, next = Continue }
            _ ->
                vm

break : VM -> VM
break vm =
    error0 "Break in program" vm