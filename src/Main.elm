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


removeZeros lc =
    if
        Maybe.withDefault '+'
            (List.head lc)
            /= '0'
    then
        lc

    else
        removeZeros
            (Maybe.withDefault []
                (List.tail lc)
            )


removeTrailingZeros n =
    let
        boo =
            1

        rev =
            List.reverse (String.toList n)

        num =
            String.fromList
                (List.reverse
                    (removeZeros
                        rev
                    )
                )
    in
    num


rounder n =
    let
        wholePart =
            Round.round 0 n

        wholePartLen =
            String.length wholePart

        fullStr =
            String.fromFloat n

        fullLen =
            String.length fullStr

        decLen =
            -- different numbers will need different integer here
            fullLen - wholePartLen - 1

        rounded =
            Debug.log ("vals: " ++ Debug.toString wholePartLen ++ ", " ++ Debug.toString decLen)
                -- different numbers will need different hard coded number
                (if decLen > 10 then
                    Round.round (decLen - 1) n

                 else
                    fullStr
                )
    in
    -- rounded
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
