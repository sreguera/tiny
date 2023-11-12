module UtilsTest exposing (..)


import Expect
import Test exposing (Test, describe, test)
import Utils

suite : Test
suite =
    describe "The aux functions"
        [ test "span works" <|
            \_ -> Expect.equal ("123", "abc") (Utils.span Char.isDigit "123abc")
        ]
