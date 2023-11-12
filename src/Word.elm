module Word exposing 
    ( Word
    , zero, fromInt, toInt, toUnsignedInt
    , neg, add, minus, mul, div
    , eq, less
    )

{-| A 16 bit word and its operations.

# Definition
@docs Word, zero, fromInt, toInt, toUnsignedInt

# Operations
@docs neg, add, minus, mul, div

# Comparisons
@docs eq, less

-}

import Bitwise


{-| A 16 bit word.
-}
type Word =
    Word Int


{-| The number 0.
-}
zero : Word
zero = fromInt 0


{-|-}
fromInt : Int -> Word
fromInt int =
    Word (Bitwise.and int 0xFFFF)


{-|-}
toInt : Word -> Int
toInt (Word int) =
    Bitwise.shiftRightBy 16 (Bitwise.shiftLeftBy 16 int)


{-|-}
toUnsignedInt : Word -> Int
toUnsignedInt (Word int) =
    int


{-|-}
neg : Word -> Word
neg (Word i1) =
    fromInt (-i1)


{-|-}
add : Word -> Word -> Word
add (Word i1) (Word i2) =
    fromInt (i1 + i2)


{-|-}
minus : Word -> Word -> Word
minus (Word i1) (Word i2) =
    fromInt (i1 - i2)


{-|-}
mul : Word -> Word -> Word
mul (Word i1) (Word i2) =
    fromInt (i1 * i2)


{-|-}
div : Word -> Word -> Maybe Word
div w1 w2 =
    if w2 == zero then
        Nothing
    else
        Just <| fromInt ((toInt w1) // (toInt w2))


{-|-}
eq : Word -> Word -> Bool
eq (Word i1) (Word i2) =
    i1 == i2


{-|-}
less : Word -> Word -> Bool
less (Word i1) (Word i2) =
    i1 < i2
