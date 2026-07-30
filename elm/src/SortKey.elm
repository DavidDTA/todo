module SortKey exposing (after, before, between, init)

import BigInt
import Char
import String


init =
    after ""


before key =
    between "" key


after key =
    between ("a" ++ key) "b"
        |> String.dropLeft 1


between key1 key2 =
    let
        digits =
            max (String.length key1) (String.length key2)

        normalized1 =
            toFixed key1 digits

        normalized2 =
            toFixed key2 digits

        mid =
            BigInt.div (BigInt.add (BigInt.mul normalized1 base) (BigInt.mul normalized2 base)) (BigInt.fromInt 2)

        midTruncated =
            BigInt.div mid base
    in
    if midTruncated == normalized1 || midTruncated == normalized2 then
        fromFixed mid (digits + 1)

    else
        fromFixed midTruncated digits


toFixed str digits =
    toFixed_ (String.toList str) digits zero


toFixed_ str digits acc =
    if digits == 0 then
        acc

    else
        let
            ( digit, nextStr ) =
                case str of
                    [] ->
                        ( zero, [] )

                    c :: tail ->
                        ( charToInt c, tail )
        in
        toFixed_ nextStr (digits - 1) (BigInt.add (BigInt.mul acc base) digit)


fromFixed num digits =
    String.fromList (fromFixed_ num digits [])


fromFixed_ num digits acc =
    if digits == 0 then
        acc

    else
        let
            ( nextNum, digit ) =
                BigInt.divmod num base
                    |> Maybe.withDefault ( zero, zero )
        in
        fromFixed_ nextNum
            (digits - 1)
            (if acc == [] && digit == zero then
                acc

             else
                intToChar digit :: acc
            )


charToInt c =
    let
        sanitized =
            if c < '!' then
                '!'

            else if c > '~' then
                '~'

            else
                c
    in
    BigInt.fromInt (Char.toCode sanitized - Char.toCode '!')


intToChar int =
    int
        |> BigInt.toString
        |> String.toInt
        |> Maybe.withDefault 0
        |> (+) (Char.toCode '!')
        |> Char.fromCode


base =
    BigInt.fromInt (Char.toCode '~' - Char.toCode '!' + 1)


zero =
    BigInt.fromInt 0


one =
    BigInt.fromInt 1
