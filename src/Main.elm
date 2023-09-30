module Main exposing (..)

-- Tiny Basic interpreter.

import Browser
import Html exposing (Html, div, text, input, form)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (value)
import Array exposing (Array)
import Dict exposing (Dict)

-- MAIN

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Address = Int

type Opcode 
  = TST Address String   -- Check if current buf starts by word or else jump to address.
  | CALL Address         -- Execute the IL subroutine at address. Save next inst in control stack.
  | RTN                  -- Return from IL subroutine to the inst at the top of the control stack.
  | DONE
  | JMP Address          -- Continue execution at the address.
  | PRS
  | PRN
  | SPC
  | NLINE
  | NXT
  | XFER
  | SAV                  -- Push current line number onto sbrstk.
  | RSTR                 -- Replace current line number with top of sbrstk.
  | CMPR
  | LIT Int              -- Push the number onto the aestk.
  | INNUM
  | FIN                  -- Return to the line collect routine.
  | ERR
  | ADD                  -- Replace top two elements of aestk by their sum.
  | SUB                  -- Replace top two elements of aestk by their difference.
  | NEG                  -- Replace top of aestk with its negative.
  | MUL                  -- Replace top two elements of aestk by their product.
  | DIV                  -- Replace top two elements of aestk by their quotient.
  | STORE                -- Place the value at top of aestk at the var below it.
  | TSTV Address         -- Test for variable and put its index on stack, or continue at address.
  | TSTN Address         -- Test for number and put it on stack, or continue at address.
  | IND                  -- Replace top of aestk by the var it indexes.
  | LST
  | INIT
  | GETLINE              -- Input a line to lbuf.
  | TSTL Address         -- Check if current buf starts by number or else jump to address.
  | INSRT                -- Insert current line buf in the line store.
  | XINIT

ucode : List Opcode
ucode =
  [                --      ;THE IL CONTROL SECTION
    INIT           -- 000  START:  INIT                  ;INITIALIZE
  , NLINE          -- 001          NLINE                 ;WRITE CRLF
  , GETLINE        -- 002  CO:     GETLINE               ;WRITE PROMPT AND GET LINE
  , TSTL 6         -- 003          TSTL    XEC           ;TEST FOR LINE NUMBER
  , INSRT          -- 004          INSERT                ;INSERT IT (MAY BE DELETE)
  , JMP 2          -- 005          JMP     CO
  , XINIT          -- 006  XEC:    XINIT                 ;INITIALIZE
                   --      ;STATEMENT EXECUTOR 
  , TST 14 "LET"   -- 007  STMT:   TST     S1,'LET'      ;IS STATEMENT A LET
  , TSTV 67        -- 008          TSTV    S17           ;YES, PLACE VAR ADDRESS ON AESTK
  , TST 67 "="     -- 009          TST     S17,'='       ;(This line originally omitted)
  , CALL 68        -- 010          CALL    EXPR          ;PLACE EXPR VALUE ON AESTK
  , DONE           -- 011          DONE                  ;REPORT ERROR IF NOT NEXT
  , STORE          -- 012          STORE                 ;STORE RESULT
  , NXT            -- 013          NXT                   ;AND SEQUENCE TO NEXT
  , TST 24 "GO"    -- 014  S1:     TST     S3,'GO'       ;GOTO OT GOSUB?
  , TST 19 "TO"    -- 015          TST     S2,'TO'       ;YES...TO, OR...SUB
  , CALL 68        -- 016          CALL    EXPR          ;GET LABEL
  , DONE           -- 017          DONE                  ;ERROR IF CR NOT NEXT
  , XFER           -- 018          XPER                  ;SET UP AND JUMP
  , TST 19 "SUB"   -- 019  S2:     TST     S17,'SUB'     ;ERROR IF NO MATCH
  , CALL 68        -- 020          CALL    EXPR          ;GET DESTINATION
  , DONE           -- 021          DONE                  ;ERROR IF CR NOT NEXT
  , SAV            -- 022          SAV                   ;SAVE RETURN LINE
  , XFER           -- 023          XPER                  ;AND JUMP
  , TST 36 "PRINT" -- 024  S3:     TST     S8,'PRINT'    ;PRINT
  , TST 33 "\""    -- 025  S4:     TST     S7,'"'        ;TEST FOR QUOTE
  , PRS            -- 026          PRS                   ;PRINT STRING
  , TST 30 ","     -- 027  S5:     TST     S6,','        ;IS THERE MORE?
  , SPC            -- 028          SPC                   ;SPACE TO NEXT ZONE
  , JMP 25         -- 039          JMP     S4            ;YES JUMP BACK
  , DONE           -- 030  S6:     DONE                  ;ERROR IF CR NOT NEXT
  , NLINE          -- 031          NLINE
  , NXT            -- 032          NXT
  , CALL 68        -- 033  S7:     CALL    EXPR
  , PRN            -- 034          PRN                   ;PRINT IT
  , JMP 27         -- 035          JMP     S5            ;IS THERE MORE?
  , TST 43 "IF"    -- 036  S8:     TST     S9,'IF'       ;IF STATEMENT
  , CALL 68        -- 037          CALL    EXPR          ;GET EXPRESSION
  , CALL 102       -- 038          CALL    RELOP         ;DETERMINE OPR AND PUT ON STK
  , CALL 68        -- 039          CALL    EXPR          ;GET EXPRESSION
  , TST 67 "THEN"  -- 040          TST     S17,'THEN'    ;(This line originally omitted)
  , CMPR           -- 041          CMPR                  ;PERFORM COMPARISON -- PERFORMS NXT IF FALSE
  , JMP 7          -- 042          JMP     STMT
  , TST 51 "INPUT" -- 043  S9:     TST     S12,'INPUT'   ;INPUT STATEMENT
  , TSTV 67        -- 044  S10:    TSTV    S17           ;GET VAR ADDRESS (Originally CALL VAR = nonexist)
  , INNUM          -- 045          INNUM                 ;MOVE NUMBER FROM TTY TO AESTK
  , STORE          -- 046          STORE                 ;STORE IT
  , TST 49 ","     -- 047          TST     S11,','       ;IS THERE MORE?
  , JMP 44         -- 048          JMP     S10           ;YES
  , DONE           -- 049  S11:    DONE                  ;MUST BE CR
  , NXT            -- 050          NXT                   ;SEQUENCE TO NEXT
  , TST 55 "RETURN"-- 051  S12:    TST     S13,'RETURN'  ;RETURN STATEMENT
  , DONE           -- 052          DONE                  ;MUST BE CR
  , RSTR           -- 053          RSTR                  ;RESTORE LINE NUMBER OF CALL
  , NXT            -- 054          NXT                   ;SEQUENCE TO NEXT STATEMENT
  , TST 57 "END"   -- 055  S13:    TST     S14,'END'
  , FIN            -- 056          FIN
  , TST 61 "LIST"  -- 057  S14:    TST     S15,'LIST'    ;LIST COMMAND
  , DONE           -- 058          DONE
  , LST            -- 059          LST
  , NXT            -- 060          NXT
  , TST 64 "RUN"   -- 061  S15:    TST     S16,'RUN'     ;RUN COMMAND
  , DONE           -- 062          DONE
  , NXT            -- 063          NXT
  , TST 67 "CLEAR" -- 064  S16:    TST     S17,'CLEAR'   ;CLEAR COMMAND
  , DONE           -- 065          DONE
  , JMP 0          -- 066          JMP     START
                   --   
  , ERR            -- 067  S17:    ERR                   ;SYNTAX ERROR
                   --   
  , TST 72 "-"     -- 068  EXPR:   TST     E0,'-'
  , CALL 83        -- 069          CALL    TERM          ;TEST FOR UNARY -.
  , NEG            -- 070          NEG                   ;GET VALUE
  , JMP 74         -- 071          JMP     E1            ;NEGATE IT
  , TST 73 "+"     -- 072  E0:     TST     E1A,'+'       ;LOOK FOR MORE
  , CALL 83        -- 073  E1A:    CALL    TERM          ;TEST FOR UNARY +
  , TST 78 "+"     -- 074  E1:     TST     E2,'+'        ;LEADING TERM
  , CALL 83        -- 075          CALL    TERM
  , ADD            -- 076          ADD
  , JMP 74         -- 077          JMP     E1
  , TST 82 "-"     -- 078  E2:     TST     E3,'-'        ;ANY MORE?
  , CALL 83        -- 079          CALL    TERM          ;DIFFERENCE TERM
  , SUB            -- 080          SUB
  , JMP 74         -- 081          JMP     E1
  , RTN            -- 082  E3:T2:  RTN                   ;ANY MORE?
  , CALL 92        -- 083  TERM:   CALL    FACT
  , TST 88 "*"     -- 084  T0:     TST     T1,"*"
  , CALL 92        -- 085          CALL    FACT          ;PRODUCT FACTOR.
  , MUL            -- 086          MUL
  , JMP 84         -- 087          JMP     T0
  , TST 82 "/"     -- 088  T1:     TST     T2,'/'
  , CALL 92        -- 089          CALL    FACT          ;QUOTIENT FACTOR.
  , DIV            -- 090          DIV
  , JMP 84         -- 091          JMP     T0
                   --   
  , TSTV 95        -- 092  FACT:   TSTV    F0
  , IND            -- 093          IND                   ;YES, GET THE VALUE.
  , RTN            -- 094          RTN
  , TSTN 97        -- 095  F0:     TSTN    F1            ;NUMBER, GET ITS VALUE.
  , RTN            -- 096          RTN
  , TST 101 "("    -- 097  F1:     TST     F2,'('        ;PARENTHESIZED EXPR.
  , CALL 68        -- 098          CALL    EXPR
  , TST 101 ")"    -- 099          TST     F2,')'
  , RTN            -- 100          RTN
  , ERR            -- 101  F2:     ERR                   ;ERROR.
                   --   
  , TST 105 "="    -- 102  RELOP:  TST     RO,'='
  , LIT 0          -- 103          LIT     0             ;=
  , RTN            -- 104          RTN
  , TST 114 "<"    -- 105  R0:     TST     R4,'<'
  , TST 109 "="    -- 106          TST     R1,'='
  , LIT 2          -- 107          LIT     2             ;<=
  , RTN            -- 108          RTN
  , TST 112 ">"    -- 109  R1:     TST     R3,'>'
  , LIT 3          -- 110          LIT     3             ;<>
  , RTN            -- 111          RTN
  , LIT 1          -- 112  R3:     LIT     1             ;<
  , RTN            -- 113          RTN
  , TST 67 ">"     -- 114  R4:     TST     S17,'>'
  , TST 118 "="    -- 115          TST     R5,'='
  , LIT 5          -- 116          LIT     5             ;>=
  , RTN            -- 117          RTN
  , TST 121 "<"    -- 118  R5:     TST     R6,'<'
  , LIT 3          -- 119          LIT     3
  , RTN            -- 120          RTN                   ;(This line originally omitted)
  , LIT 4          -- 121  R6:     LIT     4
  , RTN            -- 122          RTN
  ]

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
  }

initialVM : VM
initialVM =
  { pc = 0
  , code = Array.fromList ucode
  , lbuf = ""
  , aestk = []
  , cstk = []
  , vars = Dict.empty
  , curline = 0
  , sbrstk = []
  , lines = Dict.empty
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
  case Maybe.withDefault ERR (Array.get vm.pc vm.code) of
    INIT ->
      ( { vm | pc = vm.pc + 1 }, Cont )
    GETLINE ->
      ( { vm | pc = vm.pc + 1 }, Stop )
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
          ( { vm | pc = vm.pc + 1, aestk = b // a :: rest }, Cont )
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
  resume { vm | lbuf = s }

type alias Model = 
  { vm : VM
  , log : String
  , inp : String
  }

init : Model
init =
  { vm = resume initialVM
  , log = ""
  , inp = ""
  }

-- UPDATE

type Msg
  = Init
  | GotInput String
  | GotReturn

update : Msg -> Model -> Model
update msg model =
  case msg of
    GotInput str ->
      { model | inp = str } 
    GotReturn ->
      { vm = resumeWithInput model.vm model.inp
      , log = model.log ++ "\n" ++ model.inp
      , inp = ""
      }
    _ ->
      model

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text model.log ] 
    , div [] [ text (Debug.toString model.vm.lines) ]
    , form [ onSubmit GotReturn ] 
      [ input [ value model.inp, onInput GotInput ] [] ]
    ]
