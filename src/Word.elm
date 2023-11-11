module Word exposing (..)

import Bitwise

type Word =
    Word Int

zero : Word
zero = fromInt 0

fromInt : Int -> Word
fromInt int =
    Word (Bitwise.and int 0xFFFF)

toInt : Word -> Int
toInt (Word int) =
    Bitwise.shiftRightBy 16 (Bitwise.shiftLeftBy 16 int)

toUnsignedInt : Word -> Int
toUnsignedInt (Word int) =
    int

inc : Word -> Word
inc (Word i1) =
    fromInt (i1 + 1)

neg : Word -> Word
neg (Word i1) =
    fromInt (-i1)

add : Word -> Word -> Word
add (Word i1) (Word i2) =
    fromInt (i1 + i2)

minus : Word -> Word -> Word
minus (Word i1) (Word i2) =
    fromInt (i1 - i2)

mul : Word -> Word -> Word
mul (Word i1) (Word i2) =
    fromInt (i1 * i2)

div : Word -> Word -> Maybe Word
div (Word i1) (Word i2) =
    if i2 == 0 then
        Nothing
    else
        Just <| fromInt (i1 // i2)

eq : Word -> Word -> Bool
eq (Word i1) (Word i2) =
    i1 == i2

less : Word -> Word -> Bool
less (Word i1) (Word i2) =
    i1 < i2