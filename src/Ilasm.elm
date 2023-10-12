module Ilasm exposing (..)

import Ilvm
import Dict exposing (Dict)

type alias Label = String

type Inst 
    = TST Label String
    | CALL Label
    | RTN
    | DONE
    | JMP Label
    | PRS
    | PRN
    | SPC
    | NLINE
    | NXT
    | RUNXT
    | XFER
    | SAV
    | RSTR
    | CMPR
    | LIT Int
    | INNUM
    | FIN
    | ERR
    | ADD
    | SUB
    | NEG
    | MUL
    | DIV
    | STORE
    | TSTV Label
    | TSTN Label
    | IND
    | LST
    | INIT
    | GETLINE
    | TSTL Label
    | INSRT
    | XINIT
    | LABEL String

assemble : List Inst -> List Ilvm.Opcode
assemble insts =
    pass2 insts (pass1 insts)

type alias SymTab = Dict String Int

pass1 : List Inst -> SymTab
pass1 program =
    let
        pass1Int : List Inst -> Int -> SymTab -> SymTab
        pass1Int insts offset syms =
            case insts of
                LABEL l :: rest ->
                    pass1Int rest offset (Dict.insert l offset syms)
                _ :: rest ->
                    pass1Int rest (offset + 1) syms
                [] ->
                    syms
    in
    pass1Int program 0 Dict.empty

pass2 : List Inst -> SymTab -> List Ilvm.Opcode
pass2 insts syms =
    List.filterMap (assemble1 syms) insts

assemble1 : SymTab -> Inst -> Maybe Ilvm.Opcode
assemble1 syms inst =
    let
        lookup name = Maybe.withDefault 0 (Dict.get name syms)
    in
    case inst of
        TST label string ->
            Just <| Ilvm.TST (lookup label) string
        CALL label ->
            Just <| Ilvm.CALL (lookup label)
        RTN ->
            Just <| Ilvm.RTN
        DONE ->
            Just <| Ilvm.DONE
        JMP label ->
            Just <| Ilvm.JMP (lookup label)
        PRS ->
            Just <| Ilvm.PRS
        PRN ->
            Just <| Ilvm.PRN
        SPC ->
            Just <| Ilvm.SPC
        NLINE ->
            Just <| Ilvm.NLINE
        NXT ->
            Just <| Ilvm.NXT
        RUNXT ->
            Just <| Ilvm.RUNXT
        XFER ->
            Just <| Ilvm.XFER
        SAV ->
            Just <| Ilvm.SAV
        RSTR ->
            Just <| Ilvm.RSTR
        CMPR ->
            Just <| Ilvm.CMPR
        LIT val ->
            Just <| Ilvm.LIT val
        INNUM ->
            Just <| Ilvm.INNUM
        FIN ->
            Just <| Ilvm.FIN
        ERR ->
            Just <| Ilvm.ERR
        ADD ->
            Just <| Ilvm.ADD
        SUB ->
            Just <| Ilvm.SUB
        NEG ->
            Just <| Ilvm.NEG
        MUL ->
            Just <| Ilvm.MUL
        DIV ->
            Just <| Ilvm.DIV
        STORE ->
            Just <| Ilvm.STORE
        TSTV label ->
            Just <| Ilvm.TSTV (lookup label)
        TSTN label ->
            Just <| Ilvm.TSTN (lookup label)
        IND ->
            Just <| Ilvm.IND
        LST ->
            Just <| Ilvm.LST
        INIT ->
            Just <| Ilvm.INIT
        GETLINE ->
            Just <| Ilvm.GETLINE
        TSTL label ->
            Just <| Ilvm.TSTL (lookup label)
        INSRT ->
            Just <| Ilvm.INSRT
        XINIT ->
            Just <| Ilvm.XINIT
        LABEL _ ->
            Nothing
        
