module WordTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)

import Word


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
    describe "The Word module"
        [ fuzz (Fuzz.intRange 0 65535) "unsigned integers are supported" <|
            \rndInt -> 
                rndInt |> Word.fromInt |> Word.toUnsignedInt |> Expect.equal rndInt
        , fuzz (Fuzz.intRange -32768 32767) "signed integers are supported" <|
            \rndInt -> 
                rndInt |> Word.fromInt |> Word.toInt |> Expect.equal rndInt
        , fuzz Fuzz.int "neg works in general" <| 
            \x -> 
                Expect.equal (Word.fromInt -x) (Word.neg (Word.fromInt x))
        , fuzz2 Fuzz.int Fuzz.int "add works in general" <| 
            \x y -> 
                Expect.equal (Word.fromInt (x + y)) (Word.add (Word.fromInt x) (Word.fromInt y))
        , fuzz2 Fuzz.int Fuzz.int "minus works in general" <| 
            \x y ->
                Expect.equal (Word.fromInt (x - y)) (Word.minus (Word.fromInt x) (Word.fromInt y))
        , fuzz2 safeMultInt safeMultInt "mul works in general" <| 
            \x y ->
                Expect.equal (Word.fromInt (x * y)) (Word.mul (Word.fromInt x) (Word.fromInt y))
        , fuzz2 safeDivInt nonZeroSafeDivInt "div works in general" <| 
            \x y -> 
                Expect.equal (Just (Word.fromInt (x // y))) (Word.div (Word.fromInt x) (Word.fromInt y))
        ]