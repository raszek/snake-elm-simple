module Components exposing (..)

import Html.Styled exposing (Html, Attribute, table, styled, td, div)
import Css exposing (..)

import List

snakeTable : List (Attribute msg) -> List (Html msg) -> Html msg
snakeTable =
    styled Html.Styled.table
        [ border3 (px 3) solid (rgb 0 0 0)
        , borderCollapse collapse
        ]

snakeContainer =
    styled div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , height (vh 100)
        ]

gameOverContainer = snakeContainer

emptyCell =
    styled td
        [ border3 (px 1) solid (rgb 0 0 0)
        , width (vw 1)
        , height (vw 1)
        ]

snakeCell =
    styled emptyCell
        [ backgroundColor (rgb 0 0 0)
        ]

