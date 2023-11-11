module WordTests exposing (..)

import Expect
import Fuzz exposing (intRange)
import Test exposing (..)

import Word

suite : Test
suite =
    describe "The Word module"
        [ fuzz (intRange 0 65535) "unsigned integers are supported" <|
            \rndInt -> 
                rndInt |> Word.fromInt |> Word.toUnsignedInt |> Expect.equal rndInt
        , fuzz (intRange -32768 32767) "signed integers are supported" <|
            \rndInt -> 
                rndInt |> Word.fromInt |> Word.toInt |> Expect.equal rndInt
        ]