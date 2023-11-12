module IlvmTests exposing (..)

import Expect
import Test exposing (Test, describe, test, fuzz, fuzz2)
import Fuzz exposing (Fuzzer)

import Ilvm
import Array
import Dict
import Word


runCode : List Ilvm.Opcode -> Ilvm.VM
runCode cod =
    let
        vm = Ilvm.makeVM (Array.fromList cod)
    in
    Ilvm.resume (List.length cod) vm


runCodeInput : List Ilvm.Opcode -> String -> Ilvm.VM
runCodeInput cod str =
    let
        vm = Ilvm.makeVM (Array.fromList cod)
    in
    Ilvm.resume (List.length cod) { vm | lbuf = str }


nonZero : Fuzzer number -> Fuzzer number
nonZero = Fuzz.filter (\x -> x /= 0)


-- javascript MAX_SAFE_INTEGER is (2^53 - 1) and after that loses precission
-- so avoid multiplying number that produce something bigger.
safeMultInt : Fuzzer Int
safeMultInt = Fuzz.intRange -16000000 16000000


safeDivInt : Fuzzer Int
safeDivInt = Fuzz.intRange -32768 32767


nonZeroSafeDivInt : Fuzzer Int
nonZeroSafeDivInt = nonZero safeDivInt



suite : Test
suite =
    describe "The VM implementation" 
        [ describe "arithmetic"
            [ test "LIT works" <|
                \_ -> case runCode [ Ilvm.LIT 5 ] of
                    vm -> Expect.equal [Word.fromInt 5] vm.aestk
            , fuzz Fuzz.int "LIT works in general" <|
                \anInt -> case runCode [ Ilvm.LIT anInt ] of
                    vm -> Expect.equal [Word.fromInt anInt] vm.aestk
            , test "ADD works" <| 
                \_ -> case runCode [ Ilvm.LIT 5, Ilvm.LIT 7, Ilvm.ADD ] of
                    vm -> Expect.equal [Word.fromInt 12] vm.aestk
            , fuzz2 Fuzz.int Fuzz.int "ADD works in general" <| 
                \x y -> case runCode [ Ilvm.LIT x, Ilvm.LIT y, Ilvm.ADD ] of
                    vm -> Expect.equal [Word.fromInt (x + y)] vm.aestk
            , test "SUB works" <| 
                \_ -> case runCode [ Ilvm.LIT 5, Ilvm.LIT 7, Ilvm.SUB ] of
                    vm -> Expect.equal [Word.fromInt -2] vm.aestk
            , fuzz2 Fuzz.int Fuzz.int "SUB works in general" <| 
                \x y -> case runCode [ Ilvm.LIT x, Ilvm.LIT y, Ilvm.SUB ] of
                    vm -> Expect.equal [Word.fromInt (x - y)] vm.aestk
            , test "NEG works" <| 
                \_ -> case runCode [ Ilvm.LIT 5, Ilvm.NEG ] of
                    vm -> Expect.equal [Word.fromInt -5] vm.aestk
            , fuzz Fuzz.int "NEG works in general" <| 
                \x -> case runCode [ Ilvm.LIT x, Ilvm.NEG ] of
                    vm -> Expect.equal [Word.fromInt (-x)] vm.aestk
            , test "MUL works" <| 
                \_ -> case runCode [ Ilvm.LIT 5, Ilvm.LIT 7, Ilvm.MUL ] of
                    vm -> Expect.equal [Word.fromInt 35] vm.aestk
            , fuzz2 safeMultInt safeMultInt "MUL works in general" <| 
                \x y -> case runCode [ Ilvm.LIT x, Ilvm.LIT y, Ilvm.MUL ] of
                    vm -> Expect.equal [Word.fromInt (x * y)] vm.aestk
            , test "DIV works" <| 
                \_ -> case runCode [ Ilvm.LIT 29, Ilvm.LIT 7, Ilvm.DIV ] of
                    vm -> Expect.equal [Word.fromInt 4] vm.aestk
            , fuzz2 safeDivInt nonZeroSafeDivInt "DIV works in general" <| 
                \x y -> case runCode [ Ilvm.LIT x, Ilvm.LIT y, Ilvm.DIV ] of
                    vm -> Expect.equal [Word.fromInt (x // y)] vm.aestk
            , test "CMPR > works" <| 
                \_ -> case runCode [ Ilvm.LIT 1, Ilvm.LIT 4, Ilvm.LIT 7, Ilvm.CMPR ] of
                    vm -> Expect.equal 2 vm.pc
            , test "CMPR < works" <| 
                \_ -> case runCode [ Ilvm.LIT 1, Ilvm.LIT 1, Ilvm.LIT 7, Ilvm.CMPR ] of
                    vm -> Expect.equal 4 vm.pc
            ]
        , describe "variable access"
            [ test "STORE works" <| 
                \_ -> case runCode [ Ilvm.LIT 5, Ilvm.LIT 7, Ilvm.STORE ] of
                    vm -> Expect.equal (Just (Word.fromInt 7)) (Dict.get 5 vm.vars)
            , test "IND works" <| 
                \_ -> case runCode [ Ilvm.LIT 5, Ilvm.LIT 7, Ilvm.STORE, Ilvm.LIT 5, Ilvm.IND ] of
                    vm -> Expect.equal [Word.fromInt 7] vm.aestk
            ]
        , describe "control flow"
            [ test "JMP works" <| 
                \_ -> case runCode [ Ilvm.JMP 10 ] of
                    vm -> Expect.equal 10 vm.pc
            , test "FIN works" <| 
                \_ -> case runCode [ Ilvm.FIN ] of
                    vm -> Expect.equal 2 vm.pc
            ]
        , describe "parsing"
            [ test "TSTL continues with number" <| 
                \_ -> case runCodeInput [ Ilvm.TSTL 10 ] "123abc" of
                    vm -> Expect.equal 1 vm.pc
            , test "TSTL continues with white and number" <| 
                \_ -> case runCodeInput [ Ilvm.TSTL 10 ] "   123abc" of
                    vm -> Expect.equal 1 vm.pc
            , test "TSTL jumps with no number" <| 
                \_ -> case runCodeInput [ Ilvm.TSTL 10 ] "abc" of
                    vm -> Expect.equal 10 vm.pc
            , test "TST continues with word" <| 
                \_ -> case runCodeInput [ Ilvm.TST 10 "abc" ] "abcdef" of
                    vm -> Expect.equal 1 vm.pc
            , test "TST continues with two words" <| 
                \_ -> case runCodeInput [ Ilvm.TST 10 "abc",  Ilvm.TST 10 "def" ] "abcdef" of
                    vm -> Expect.equal 2 vm.pc
            , test "TST jumps with no word" <| 
                \_ -> case runCodeInput [ Ilvm.TST 10 "abx" ] "abcdef" of
                    vm -> Expect.equal 10 vm.pc
            , test "TSTV puts var in stack" <| 
                \_ -> case runCodeInput [ Ilvm.TSTV 10 ] "D" of
                    vm -> Expect.equal [Word.fromInt 3] vm.aestk
            , test "TSTN puts numbers in stack" <| 
                \_ -> case runCodeInput [ Ilvm.TSTN 10, Ilvm.TSTN 10 ] "31 4" of
                    vm -> Expect.equal [Word.fromInt 4, Word.fromInt 31] vm.aestk
            , test "TSTN jumps if no number" <| 
                \_ -> case runCodeInput [ Ilvm.TSTN 10 ] "abc" of
                    vm -> Expect.equal 10 vm.pc
            ]
        ]


