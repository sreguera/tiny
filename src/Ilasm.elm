module Ilasm exposing (..)

import Ilvm

type alias Label = Int

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
    List.filterMap assemble1 insts

assemble1 : Inst -> Maybe Ilvm.Opcode
assemble1 inst =
    case inst of
        TST label string ->
            Just <| Ilvm.TST label string
        CALL label ->
            Just <| Ilvm.CALL label
        RTN ->
            Just <| Ilvm.RTN
        DONE ->
            Just <| Ilvm.DONE
        JMP label ->
            Just <| Ilvm.JMP label
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
            Just <| Ilvm.TSTV label
        TSTN label ->
            Just <| Ilvm.TSTN label
        IND ->
            Just <| Ilvm.IND
        LST ->
            Just <| Ilvm.LST
        INIT ->
            Just <| Ilvm.INIT
        GETLINE ->
            Just <| Ilvm.GETLINE
        TSTL label ->
            Just <| Ilvm.TSTL label
        INSRT ->
            Just <| Ilvm.INSRT
        XINIT ->
            Just <| Ilvm.XINIT
        LABEL _ ->
            Nothing
        
