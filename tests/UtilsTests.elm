module UtilsTests exposing (..)


import Expect
import Test exposing (Test, describe, test, fuzz)
import Fuzz 

import Utils


spanPredicate : Char -> Bool
spanPredicate = Char.isDigit

suite : Test
suite =
    describe "The utility function"
        [ describe "span" 
            [ test "works with an example" <|
                \_ -> Expect.equal ("123", "abc") (Utils.span Char.isDigit "123abc")
            , fuzz Fuzz.string "parts the string in two" <|
                \s -> 
                    let
                        (front, back) = Utils.span spanPredicate s
                    in
                    Expect.equal s (front ++ back)
            , fuzz Fuzz.string "all elements in the front comply with the predicate" <|
                \s -> 
                    let
                        (front, _) = Utils.span spanPredicate s
                    in
                    Expect.equal True (String.all spanPredicate front)
            , fuzz Fuzz.string "the first element in the back does not comply with the predicate" <|
                \s -> 
                    let
                        (_, back) = Utils.span spanPredicate s
                    in
                    case String.uncons back of
                        Just (c, _) ->
                            Expect.equal False (spanPredicate c)
                        Nothing ->
                            Expect.pass
            ]
        ]
