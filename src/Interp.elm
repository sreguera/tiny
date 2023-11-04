module Interp exposing (..)

import Ilvm exposing (VM, makeVM)
import Ilasm exposing (Inst(..), assemble)
import Array

initialVM : VM
initialVM = makeVM (Array.fromList (assemble interp))

interp : List Inst
interp =
    [ -- THE IL CONTROL SECTION
      LABEL "START"
    , INIT              -- INITIALIZE
    , NLINE             -- WRITE CRLF
    , LABEL "CO"
    , GETLINE           -- WRITE PROMPT AND GET LINE
    , TSTL "XEC"        -- TEST FOR LINE NUMBER
    , INSRT             -- INSERT IT (MAY BE DELETE)
    , JMP "CO"       
    , LABEL "XEC"
    , XINIT             -- INITIALIZE
      -- STATEMENT EXECUTOR 
    , LABEL "STMT"
    , TST "S1" "LET"    -- IS STATEMENT A LET
    , TSTV "S17"        -- YES, PLACE VAR ADDRESS ON AESTK
    , TST "S17" "="     -- (This line originally omitted)
    , CALL "EXPR"       -- PLACE EXPR VALUE ON AESTK
    , DONE              -- REPORT ERROR IF NOT NEXT
    , STORE             -- STORE RESULT
    , NXT               -- AND SEQUENCE TO NEXT
    , LABEL "S1"
    , TST "S3" "GO"     -- GOTO OT GOSUB?
    , TST "S2" "TO"     -- YES...TO, OR...SUB
    , CALL "EXPR"       -- GET LABEL
    , DONE              -- ERROR IF CR NOT NEXT
    , XFER              -- SET UP AND JUMP
    , LABEL "S2"
    , TST "S17" "SUB"   -- ERROR IF NO MATCH
    , CALL "EXPR"       -- GET DESTINATION
    , DONE              -- ERROR IF CR NOT NEXT
    , SAV               -- SAVE RETURN LINE
    , XFER              -- AND JUMP
    , LABEL "S3"
    , TST "S8" "PRINT"  -- PRINT
    , LABEL "S4"
    , TST "S7" "\""     -- TEST FOR QUOTE
    , PRS               -- PRINT STRING
    , LABEL "S5"
    , TST "S6" ","      -- IS THERE MORE?
    , SPC               -- SPACE TO NEXT ZONE
    , JMP "S4"          -- YES JUMP BACK
    , LABEL "S6"
    , DONE              -- ERROR IF CR NOT NEXT
    , NLINE
    , NXT
    , LABEL "S7"
    , CALL "EXPR"
    , PRN               -- PRINT IT
    , JMP "S5"          -- IS THERE MORE?
    , LABEL "S8"
    , TST "S9" "IF"     -- IF STATEMENT
    , CALL "EXPR"       -- GET EXPRESSION
    , CALL "RELOP"      -- DETERMINE OPR AND PUT ON STK
    , CALL "EXPR"       -- GET EXPRESSION
    , TST "S17" "THEN"  -- (This line originally omitted)
    , CMPR              -- PERFORM COMPARISON, PERFORMS NXT IF FALSE
    , JMP "STMT"
    , LABEL "S9"
    , TST "S12" "INPUT" -- INPUT STATEMENT
    , LABEL "S10"
    , TSTV "S17"        -- GET VAR ADDRESS (Originally CALL VAR = nonexist)
    , INNUM             -- MOVE NUMBER FROM TTY TO AESTK
    , STORE             -- STORE IT
    , TST "S11" ","     -- IS THERE MORE?
    , JMP "S10"         -- YES
    , LABEL "S11"
    , DONE              -- MUST BE CR
    , NXT               -- SEQUENCE TO NEXT
    , LABEL "S12"
    , TST "S13" "RETURN"-- RETURN STATEMENT
    , DONE              -- MUST BE CR
    , RSTR              -- RESTORE LINE NUMBER OF CALL
    , NXT               -- SEQUENCE TO NEXT STATEMENT
    , LABEL "S13"
    , TST "S14A" "REM"
    , NXT
    , LABEL "S14A"
    , TST "S14" "END"
    , FIN
    , LABEL "S14"
    , TST "S15" "LIST"  -- LIST COMMAND
    , DONE
    , LST
    , NXT
    , LABEL "S15"
    , TST "S16" "RUN"   -- RUN COMMAND
    , DONE
    , RUNXT
    , LABEL "S16"
    , TST "S17" "CLEAR" -- CLEAR COMMAND
    , DONE
    , JMP "START"
    , LABEL "S17"
    , ERR               -- SYNTAX ERROR
    , LABEL "EXPR"
    , TST "E0" "-"
    , CALL "TERM"       -- TEST FOR UNARY -
    , NEG               -- GET VALUE
    , JMP "E1"          -- NEGATE IT
    , LABEL "E0"
    , TST "E1A" "+"     -- LOOK FOR MORE
    , LABEL "E1A"
    , CALL "TERM"       -- TEST FOR UNARY +
    , LABEL "E1"
    , TST "E2" "+"      -- LEADING TERM
    , CALL "TERM" 
    , ADD
    , JMP "E1"
    , LABEL "E2"
    , TST "E3" "-"      -- ANY MORE?
    , CALL "TERM"       -- DIFFERENCE TERM
    , SUB
    , JMP "E1"
    , LABEL "E3"
    , LABEL "T2"
    , RTN               -- ANY MORE?
    , LABEL "TERM"
    , CALL "FACT"
    , LABEL "T0"
    , TST "T1" "*"
    , CALL "FACT"       -- PRODUCT FACTOR.
    , MUL
    , JMP "T0"
    , LABEL "T1"
    , TST "T2" "/"
    , CALL "FACT"       -- QUOTIENT FACTOR.
    , DIV
    , JMP "T0"
    , LABEL "FACT"
    , TSTV "F0"
    , IND               -- YES, GET THE VALUE.
    , RTN
    , LABEL "F0"
    , TSTN "F1"         -- NUMBER, GET ITS VALUE.
    , RTN
    , LABEL "F1"
    , TST "F2" "("      -- PARENTHESIZED EXPR.
    , CALL "EXPR"
    , TST "F2" ")"
    , RTN
    , LABEL "F2"
    , ERR               -- ERROR.
    , LABEL "RELOP"
    , TST "R0" "="
    , LIT 0             -- =
    , RTN
    , LABEL "R0"
    , TST "R4" "<"
    , TST "R1" "="
    , LIT 2             -- <=
    , RTN
    , LABEL "R1"
    , TST "R3" ">"
    , LIT 3             -- <>
    , RTN
    , LABEL "R3"
    , LIT 1             -- <
    , RTN
    , LABEL "R4"
    , TST "S17" ">"
    , TST "R5" "="
    , LIT 5             -- >=
    , RTN
    , LABEL "R5"
    , TST "R6" "<"
    , LIT 3             -- ><
    , RTN 
    , LABEL "R6"
    , LIT 4             -- >
    , RTN
    ]

