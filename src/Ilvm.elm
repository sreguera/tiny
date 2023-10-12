module Ilvm exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)

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
    | CMPR
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
    , aestk : List Int
    , cstk : List Address
    , vars : Dict Int Int
    , curline : Int
    , sbrstk : List Int
    , lines : Dict Int String
    , output : String
    , resume : Resumer
    }

type Resumer = Resumer (VM -> VM)

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
    , resume = Resumer identity
    }

type Next
    = Cont
    | Stop

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

exec1 : VM -> (VM, Next)
exec1 vm =
    let
        error : String -> (VM, Next)
        error msg =
            let
                line = String.concat ["\n!", String.fromInt vm.curline, ": ", msg, "\n"]
            in
            ( { vm | pc = 2, output = String.append vm.output line }, Cont )
    in
    case Maybe.withDefault ERR (Array.get vm.pc vm.code) of
        INIT ->
            ( { vm | pc = vm.pc + 1
                , lbuf = ""
                , aestk = []
                , cstk = []
                , vars = Dict.empty
                , curline = 0
                , sbrstk = []
                , lines = Dict.empty 
            }, Cont )

        XINIT ->
            ( { vm | pc = vm.pc + 1, aestk = [], cstk = [] }, Cont )

        NXT ->
            if vm.curline == 0 then
                ( { vm | pc = 2 }, Cont )
            else
                let
                    next = List.minimum <| List.filter (\x -> x > vm.curline) <| Dict.keys vm.lines
                in
                case next of
                    Just l ->
                        ( { vm | pc = 7, curline = l, lbuf = Maybe.withDefault "" (Dict.get l vm.lines) }, Cont )
                    _ ->
                        ( { vm | pc = 2, curline = 0, lbuf = "" }, Cont )

        RUNXT ->
            let
                next = List.minimum <| List.filter (\x -> x > vm.curline) <| Dict.keys vm.lines
            in
            case next of
                Just l ->
                    ( { vm | pc = 7, curline = l, lbuf = Maybe.withDefault "" (Dict.get l vm.lines) }, Cont )
                _ ->
                    ( { vm | pc = 2, curline = 0, lbuf = "" }, Cont )

        XFER ->
            case vm.aestk of
                a :: rest ->
                    case Dict.get a vm.lines of
                        Just code ->
                            ( { vm | pc = 7, curline = a, lbuf = code, aestk = rest }, Cont )
                        _ ->
                            ( vm, Cont ) -- TODO: Error 
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        DONE ->
            if String.isEmpty (String.trim vm.lbuf) then
                ( { vm | pc = vm.pc + 1 }, Cont )
            else
                error "Syntax error"
        
        ERR ->
            error "Syntax error"

        GETLINE ->
            ( { vm | pc = vm.pc + 1, resume = Resumer identity }, Stop )

        INNUM ->
            let
                parseNum : VM -> VM
                parseNum vm0 =
                    case String.toInt vm0.lbuf of
                        Just n ->
                            { vm0 | lbuf = "", aestk = n :: vm0.aestk }
                        Nothing ->
                            vm0 -- TODO: Error
            in
            ( { vm | pc = vm.pc + 1, resume = Resumer parseNum }, Stop )

        FIN ->
            ( { vm | pc = 2 }, Cont )

        JMP addr ->
            ( { vm | pc = addr }, Cont )

        CALL addr ->
            ( { vm | pc = addr, cstk = vm.pc + 1 :: vm.cstk }, Cont )

        RTN ->
            case vm.cstk of
                a :: rest ->
                    ( { vm | pc = a, cstk = rest }, Cont )
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        SAV ->
            ( { vm | pc = vm.pc + 1, sbrstk = vm.curline :: vm.sbrstk }, Cont )

        RSTR ->
            case vm.sbrstk of
                a :: rest ->
                    ( { vm | pc = vm.pc + 1, sbrstk = rest, curline = a }, Cont )
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        INSRT ->
            let
                (lnums, code) = span Char.isDigit vm.lbuf
            in
                case String.toInt lnums of
                    Just lnum ->
                        ( { vm | pc = vm.pc + 1, lbuf = "", lines = Dict.insert lnum code vm.lines }, Cont )
                    Nothing ->
                        ( { vm | pc = vm.pc + 1, lbuf = "" }, Cont ) -- TODO: Error 

        TSTL addr ->
            let
                lbuf = String.trimLeft vm.lbuf
            in
            case String.uncons lbuf of
                Just (n, _) ->
                    if Char.isDigit n then
                        ( { vm | lbuf = lbuf, pc = vm.pc + 1 }, Cont )
                    else
                        ( { vm | lbuf = lbuf, pc = addr }, Cont )
                Nothing ->
                    ( { vm | lbuf = lbuf, pc = vm.pc + 1 }, Cont ) -- TODO: Error

        TST addr str ->
            let
                lbuf = String.trimLeft vm.lbuf
            in
            if String.startsWith str lbuf then
                ( { vm | lbuf = String.dropLeft (String.length str) lbuf, pc = vm.pc + 1 }, Cont )
            else
                ( { vm | lbuf = lbuf, pc = addr }, Cont )

        TSTV addr ->
            let
                lbuf = String.trimLeft vm.lbuf
            in
            case String.uncons lbuf of
                Just (v, _) ->
                    if Char.isUpper v then
                        ( { vm | lbuf = String.dropLeft 1 lbuf, aestk = (Char.toCode v - Char.toCode 'A') :: vm.aestk, pc = vm.pc + 1 }, Cont )
                    else
                        ( { vm | lbuf = lbuf, pc = addr }, Cont )
                Nothing ->
                    ( { vm | lbuf = lbuf, pc = vm.pc + 1 }, Cont ) -- TODO: Error

        TSTN addr ->
            let
                lbuf = String.trimLeft vm.lbuf
                (lnums, rest) = span Char.isDigit lbuf
            in
            case String.toInt lnums of
                Just lnum ->
                    ( { vm | pc = vm.pc + 1, lbuf = rest, aestk = lnum :: vm.aestk }, Cont )
                Nothing ->
                    ( { vm | pc = addr, lbuf = lbuf }, Cont ) -- TODO: Error 

        PRS ->
            let
                (out, rest) = span (\c -> c /= '"') vm.lbuf
                rest1 = String.dropLeft 1 rest
            in
            ( { vm | pc = vm.pc + 1, lbuf = rest1, output = String.append vm.output out }, Cont )

        PRN ->
            case vm.aestk of
                a :: rest ->
                    ( { vm | pc = vm.pc + 1, aestk = rest, output = String.append vm.output (String.fromInt a) }, Cont )
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        SPC ->
            ( { vm | pc = vm.pc + 1, output = String.append vm.output " " }, Cont )

        NLINE ->
            ( { vm | pc = vm.pc + 1, output = String.append vm.output "\n" }, Cont )

        LST ->
            let
                lines = String.concat (List.map (\(k, v) -> String.fromInt k ++ " " ++ v ++ "\n") (Dict.toList vm.lines))
            in
            ( { vm | pc = vm.pc + 1, output = String.append vm.output lines }, Cont ) 

        STORE ->
            case vm.aestk of
                a :: b :: rest ->
                    ( { vm | pc = vm.pc + 1, aestk = rest, vars = Dict.insert b a vm.vars }, Cont )
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        IND ->
            case vm.aestk of
                a :: rest ->
                    case Dict.get a vm.vars of
                        Just v ->
                            ( { vm | pc = vm.pc + 1, aestk = v :: rest }, Cont )
                        _ ->
                            ( vm, Cont ) -- TODO: Error 
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        LIT val ->
            ( { vm | pc = vm.pc + 1, aestk = val :: vm.aestk }, Cont )

        ADD ->
            case vm.aestk of
                a :: b :: rest ->
                    ( { vm | pc = vm.pc + 1, aestk = a + b :: rest }, Cont )
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        SUB ->
            case vm.aestk of
                a :: b :: rest ->
                    ( { vm | pc = vm.pc + 1, aestk = b - a :: rest }, Cont )
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        NEG ->
            case vm.aestk of
                a :: rest ->
                    ( { vm | pc = vm.pc + 1, aestk =  -a :: rest }, Cont )
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        MUL ->
            case vm.aestk of
                a :: b :: rest ->
                    ( { vm | pc = vm.pc + 1, aestk = a * b :: rest }, Cont )
                _ ->
                    ( vm, Cont ) -- TODO: Error 

        DIV ->
            case vm.aestk of
                a :: b :: rest ->
                    ( { vm | pc = vm.pc + 1, aestk = b // a :: rest }, Cont ) -- TODO: div by 0
                _ ->
                    ( vm, Cont ) -- TODO: Error

        _ ->
            ( { vm | pc = vm.pc + 1 }, Cont )

execN : VM -> Int -> (VM, Next)
execN vm n =
    if n == 0 then
        ( vm, Cont )
    else
        case exec1 vm of
            ( vm1, Stop ) ->
                ( vm1, Stop)
            ( vm1, Cont ) ->
                execN vm1 (n - 1)

resume : VM -> VM
resume vm =
    case exec1 vm of
        ( vm1, Stop ) ->
            vm1
        ( vm1, Cont ) ->
            resume vm1

resumeWithInput : VM -> String -> VM
resumeWithInput vm s =
    resume <| 
        case vm.resume of 
            Resumer f ->
                f { vm | lbuf = s }
