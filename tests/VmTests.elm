module VmTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Main
import Array
import Dict

runCode : List Main.Opcode -> (Main.VM, Main.Next)
runCode cod =
    let
        vm = Main.initialVM
    in
        Main.execN { vm | code = Array.fromList cod } (List.length cod)

runCodeInput : List Main.Opcode -> String -> (Main.VM, Main.Next)
runCodeInput cod str =
    let
        vm = Main.initialVM
    in
        Main.execN { vm | code = Array.fromList cod, lbuf = str } (List.length cod)

suite : Test
suite =
    describe "The VM implementation" 
        [ test "LIT works" <| 
            \_ -> case runCode [ Main.LIT 5 ] of
                    (vm, _) -> Expect.equal [5] vm.aestk
        , test "ADD works" <| 
            \_ -> case runCode [ Main.LIT 5, Main.LIT 7, Main.ADD ] of
                    (vm, _) -> Expect.equal [12] vm.aestk
        , test "SUB works" <| 
            \_ -> case runCode [ Main.LIT 5, Main.LIT 7, Main.SUB ] of
                    (vm, _) -> Expect.equal [-2] vm.aestk
        , test "NEG works" <| 
            \_ -> case runCode [ Main.LIT 5, Main.NEG ] of
                    (vm, _) -> Expect.equal [-5] vm.aestk
        , test "MUL works" <| 
            \_ -> case runCode [ Main.LIT 5, Main.LIT 7, Main.MUL ] of
                    (vm, _) -> Expect.equal [35] vm.aestk
        , test "DIV works" <| 
            \_ -> case runCode [ Main.LIT 29, Main.LIT 7, Main.DIV ] of
                    (vm, _) -> Expect.equal [4] vm.aestk
        , test "STORE works" <| 
            \_ -> case runCode [ Main.LIT 5, Main.LIT 7, Main.STORE ] of
                    (vm, _) -> Expect.equal (Just 7) (Dict.get 5 vm.vars)
        , test "IND works" <| 
            \_ -> case runCode [ Main.LIT 5, Main.LIT 7, Main.STORE, Main.LIT 5, Main.IND ] of
                    (vm, _) -> Expect.equal [7] vm.aestk
        , test "JMP works" <| 
            \_ -> case runCode [ Main.JMP 10 ] of
                    (vm, _) -> Expect.equal 10 vm.pc
        , test "FIN works" <| 
            \_ -> case runCode [ Main.FIN ] of
                    (vm, _) -> Expect.equal 2 vm.pc
        , test "TSTL continues with number" <| 
            \_ -> case runCodeInput [ Main.TSTL 10 ] "123abc" of
                    (vm, _) -> Expect.equal 1 vm.pc
        , test "TSTL continues with white and number" <| 
            \_ -> case runCodeInput [ Main.TSTL 10 ] "   123abc" of
                    (vm, _) -> Expect.equal 1 vm.pc
        , test "TSTL jumps with no number" <| 
            \_ -> case runCodeInput [ Main.TSTL 10 ] "abc" of
                    (vm, _) -> Expect.equal 10 vm.pc
        , test "TST continues with word" <| 
            \_ -> case runCodeInput [ Main.TST 10 "abc" ] "abcdef" of
                    (vm, _) -> Expect.equal 1 vm.pc
        , test "TST continues with two words" <| 
            \_ -> case runCodeInput [ Main.TST 10 "abc",  Main.TST 10 "def" ] "abcdef" of
                    (vm, _) -> Expect.equal 2 vm.pc
        , test "TST jumps with no word" <| 
            \_ -> case runCodeInput [ Main.TST 10 "abx" ] "abcdef" of
                    (vm, _) -> Expect.equal 10 vm.pc
        , test "TSTV puts var in stack" <| 
            \_ -> case runCodeInput [ Main.TSTV 10 ] "D" of
                    (vm, _) -> Expect.equal [3] vm.aestk
        , test "TSTN puts numbers in stack" <| 
            \_ -> case runCodeInput [ Main.TSTN 10, Main.TSTN 10 ] "31 4" of
                    (vm, _) -> Expect.equal [4, 31] vm.aestk
        , test "TSTN jumps if no number" <| 
            \_ -> case runCodeInput [ Main.TSTN 10 ] "abc" of
                    (vm, _) -> Expect.equal 10 vm.pc
        ]

suite2 : Test
suite2 =
    describe "The aux functions"
        [ test "span works" <|
            \_ -> Expect.equal ("123", "abc") (Main.span Char.isDigit "123abc")
        ]

