module Position exposing (..)

type alias Position =
    { x : Int
    , y : Int
    }

isPositionEqual : Position -> Position -> Bool
isPositionEqual positionOne positionTwo =
    positionOne.x == positionTwo.x && positionOne.y == positionTwo.y
