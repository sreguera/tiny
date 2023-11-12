module Ilasm exposing
    ( Inst(..)
    , assemble
    )

{-| An assembler for the IL virtual machine.

# Instructions
@docs Inst

# Assembler
@docs assemble

-}

import Ilvm
import Dict exposing (Dict)
import Result exposing (andThen)
import List
import Utils

type alias Label = String


{-| An assembly instruction corresponds one to one with an IL VM opcode.
The only exception is the LABEL pseudo-instruction. 

The difference between instructions and opcodes is that instructions take
address arguments as symbols, while opcodes use numeric offsets.

-}
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


{-| Assemble a list of instructions into the equivalent list of opcodes.
Returns an error instead:

- If a label is used without being defined.
- If a label is duplicated.

-}
assemble : List Inst -> Result String (List Ilvm.Opcode)
assemble insts =
    pass1 insts 
        |> andThen (pass2 insts)


type alias SymTab = Dict String Int


pass1 : List Inst -> Result String SymTab
pass1 program =
    let
        pass1Int : List Inst -> Int -> SymTab -> Result String SymTab
        pass1Int insts offset syms =
            case insts of
                LABEL name :: rest ->
                    if Dict.member name syms then
                        Err <| "ILASM: Duplicated label " ++ name
                    else
                        pass1Int rest offset (Dict.insert name offset syms)
                _ :: rest ->
                    pass1Int rest (offset + 1) syms
                [] ->
                    Ok syms
    in
    pass1Int program 0 Dict.empty


pass2 : List Inst -> SymTab -> Result String (List Ilvm.Opcode)
pass2 insts syms =
    List.map (encode syms) insts
        |> Utils.combine
        |> Result.map List.concat


{-| Encode one assembly instruction into VM opcodes. Uses the symbol
table to lookup label values. Returns an error:

- If a label is not found in the symbol table.

-}
encode : SymTab -> Inst -> Result String (List Ilvm.Opcode)
encode syms inst =
    let
        encodeLabel : (Int -> Ilvm.Opcode) -> Label -> Result String (List Ilvm.Opcode)
        encodeLabel fop name =
            case Dict.get name syms of
                Just val ->
                    Ok [ fop val ]
                Nothing ->
                    Err <| "ILASM: Undefined label " ++ name

        encodeNumber : (Int -> Ilvm.Opcode) -> Int -> Result String (List Ilvm.Opcode)
        encodeNumber fop n =
            Ok [ fop n ]

        encodeSimple : Ilvm.Opcode -> Result String (List Ilvm.Opcode)
        encodeSimple op =
            Ok [ op ]

        encodeNothing : Result String (List Ilvm.Opcode)
        encodeNothing =
            Ok []

    in
    case inst of
        TST label string ->
            encodeLabel ((Utils.flip Ilvm.TST) string) label
        CALL label ->
            encodeLabel Ilvm.CALL label
        RTN ->
            encodeSimple Ilvm.RTN
        DONE ->
            encodeSimple Ilvm.DONE
        JMP label ->
            encodeLabel Ilvm.JMP label
        PRS ->
            encodeSimple Ilvm.PRS
        PRN ->
            encodeSimple Ilvm.PRN
        SPC ->
            encodeSimple Ilvm.SPC
        NLINE ->
            encodeSimple Ilvm.NLINE
        NXT ->
            encodeSimple Ilvm.NXT
        RUNXT ->
            encodeSimple Ilvm.RUNXT
        XFER ->
            encodeSimple Ilvm.XFER
        SAV ->
            encodeSimple Ilvm.SAV
        RSTR ->
            encodeSimple Ilvm.RSTR
        CMPR ->
            encodeSimple Ilvm.CMPR
        LIT val ->
            encodeNumber Ilvm.LIT val
        INNUM ->
            encodeSimple Ilvm.INNUM
        FIN ->
            encodeSimple Ilvm.FIN
        ERR ->
            encodeSimple Ilvm.ERR
        ADD ->
            encodeSimple Ilvm.ADD
        SUB ->
            encodeSimple Ilvm.SUB
        NEG ->
            encodeSimple Ilvm.NEG
        MUL ->
            encodeSimple Ilvm.MUL
        DIV ->
            encodeSimple Ilvm.DIV
        STORE ->
            encodeSimple Ilvm.STORE
        TSTV label ->
            encodeLabel Ilvm.TSTV label
        TSTN label ->
            encodeLabel Ilvm.TSTN label
        IND ->
            encodeSimple Ilvm.IND
        LST ->
            encodeSimple Ilvm.LST
        INIT ->
            encodeSimple Ilvm.INIT
        GETLINE ->
            encodeSimple Ilvm.GETLINE
        TSTL label ->
            encodeLabel Ilvm.TSTL label
        INSRT ->
            encodeSimple Ilvm.INSRT
        XINIT ->
            encodeSimple Ilvm.XINIT
        LABEL _ ->
            encodeNothing
        
