module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, hr, li, p, text, ul)
import Html.Events exposing (onClick)
import Round


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


sep =
    "  -   "


step =
    0.123456789


removeZeros : List Char -> List Char
removeZeros lc =
    let
        doesNotStartWithZero =
            case List.head lc of
                Nothing ->
                    True

                Just val ->
                    val /= '0'
    in
    if doesNotStartWithZero then
        lc

    else
        removeZeros
            (Maybe.withDefault []
                (List.tail lc)
            )


removeTrailingZeros : String -> String
removeTrailingZeros n =
    let
        rev =
            List.reverse (String.toList n)
    in
    String.fromList
        (List.reverse
            (removeZeros
                rev
            )
        )


rounder : Float -> String
rounder n =
    let
        wholePartLen =
            String.length
                (Round.round 0 n)

        fullStr =
            String.fromFloat n

        fullLen =
            String.length fullStr

        decLen =
            fullLen - wholePartLen - 1

        rounded =
            if decLen > 10 then
                Round.round (decLen - 2) n

            else
                fullStr
    in
    removeTrailingZeros rounded


view model =
    div []
        [ div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [] [ text (String.fromInt model) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        , hr []
            []
        , p
            []
            [ text (String.fromFloat step) ]
        , ul []
            (List.map
                (\nx ->
                    let
                        zz =
                            nx + step
                    in
                    li []
                        [ text
                            ("a "
                                ++ String.fromFloat nx
                                ++ sep
                                ++ String.fromFloat zz
                                ++ " gives "
                                ++ rounder zz
                            )
                        ]
                )
                (List.map
                    (\n -> toFloat n / 5)
                    (List.range (model + 1) (model + 150))
                )
            )
        ]
