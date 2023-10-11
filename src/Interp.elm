module Interp exposing (..)

import Ilvm exposing (VM, makeVM, Opcode(..))
import Array

initialVM : VM
initialVM = makeVM (Array.fromList ucode)

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
  , RUNXT          -- 063          NXT
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

