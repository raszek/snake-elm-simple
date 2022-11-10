module Main exposing (..)

import Browser
import Debug
import Html.Styled exposing (Html, button, div, text, tr, td, h1, toUnstyled, text)
import Components exposing (snakeTable, emptyCell, foodCell, snakeContainer, gameOverContainer, snakeCell)
import Html.Events exposing (onClick)
import List
import Util
import Time
import Json.Decode as Decode
import Browser.Events as Events
import Position exposing (Position, isPositionEqual)
import Random

main =
  Browser.element 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view >> toUnstyled
  }


type alias Grid =
    { columns : Int
    , rows : Int
    }

type SnakeMoveDirection 
    = Top
    | Bottom
    | Right
    | Left

type GameStatus = Running | GameOver

type alias SnakeHead = Position

type alias SnakeTail = List Position

type alias Food = Position

type alias Snake =
    { head : SnakeHead
    , moveDirection: SnakeMoveDirection
    , tail : SnakeTail
    }

type alias Model =
    { grid : Grid
    , snake : Snake
    , status: GameStatus
    , food: Food
    }

initGrid : Grid
initGrid =
    { columns = 15
    , rows = 15
    }

initSnake : Snake
initSnake =
    { head = initSnakeHead
    , moveDirection = Bottom
    , tail = initSnakeTail
    }

initSnakeHead : Position
initSnakeHead =
    { x = initGrid.columns // 2
    , y = initGrid.rows // 2
    }

initSnakeTail : List Position
initSnakeTail =
    [ { x = initSnakeHead.x, y = initSnakeHead.y - 1 }
    , { x = initSnakeHead.x, y = initSnakeHead.y - 2 }
    ]

initData : Model
initData =
    { grid = initGrid
    , snake = initSnake
    , status = Running
    , food = { x = 0, y = 0 }
    }

init: () -> (Model, Cmd Msg)
init _ =
    generateFood initData

isPositionOutside : Position -> Grid -> Bool
isPositionOutside position grid 
    =  position.x < 1
    || position.y < 1
    || position.x > grid.columns
    || position.y > grid.rows

moveSnake : Model -> (Model, Cmd Msg)
moveSnake model =
    let
        updatedSnakeHead = changeSnakeHeadPosition model.snake
    in
        if hasSnakeCollision model updatedSnakeHead then
            ( { model | status = GameOver }
            , Cmd.none
            )
        else if isPositionEqual updatedSnakeHead model.food then
            generateFood { model | snake = extendSnake model.snake }
        else
            ( {model | snake = changeSnakePosition model.snake}
            , Cmd.none
            )

extendSnake : Snake -> Snake
extendSnake snake =
    let
        newHeadPosition = changeSnakeHeadPosition snake
        newTailPosition = [snake.head] ++ snake.tail
    in
        { snake | head = newHeadPosition, tail = newTailPosition }

hasSnakeCollision : Model -> SnakeHead -> Bool
hasSnakeCollision model updatedHeadPosition 
    =  isPositionOutside updatedHeadPosition model.grid
    || isSnakeTailPosition model.snake.tail updatedHeadPosition


changeTailPosition : SnakeTail -> SnakeHead -> SnakeTail
changeTailPosition tail oldHeadPosition =
    [oldHeadPosition] ++ (Util.dropTail 1 tail)

changeSnakePosition : Snake -> Snake
changeSnakePosition snake =
    let
        newHeadPosition = changeSnakeHeadPosition snake
        newTailPosition = changeTailPosition snake.tail snake.head
    in
        { snake | head = newHeadPosition, tail = newTailPosition }

changeSnakeHeadPosition : Snake -> SnakeHead
changeSnakeHeadPosition snake =
    let
        direction = snake.moveDirection
        x = snake.head.x
        y = snake.head.y
    in
        case direction of
            Top -> {x = x, y = y - 1}
            Bottom -> {x = x, y = y + 1}
            Right -> {x = x + 1, y = y}
            Left -> {x = x - 1, y = y}

changeSnakeDirection : Model -> Maybe SnakeMoveDirection -> (Model, Cmd Msg)
changeSnakeDirection model maybeMove =
    let 
        updatedModel =
            case maybeMove of
                Just move -> { model | snake = updateSnakeDirection model.snake move } 
                Nothing -> model
    in
        ( updatedModel
        , Cmd.none
        )

updateSnakeDirection : Snake -> SnakeMoveDirection -> Snake
updateSnakeDirection snake move =
    if isOppositeDirection snake.moveDirection move then
        snake
    else
        { snake | moveDirection = move }

isOppositeDirection : SnakeMoveDirection -> SnakeMoveDirection -> Bool
isOppositeDirection oldDirection newDirection =
    getOppositeDirection newDirection == oldDirection

getOppositeDirection : SnakeMoveDirection -> SnakeMoveDirection
getOppositeDirection direction =
    case direction of
        Top -> Bottom
        Bottom -> Top
        Right -> Left
        Left -> Right
        
type Msg 
    = MoveSnake Time.Posix
    | ChangeDirection (Maybe SnakeMoveDirection)
    | PutFood Food

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MoveSnake _ ->
            moveSnake model
        ChangeDirection maybeMove ->
            changeSnakeDirection model maybeMove
        PutFood foodPosition ->
            putFood model foodPosition

generateFood : Model -> (Model, Cmd Msg)
generateFood model =
    ( model
    , Random.generate PutFood positionGenerator
    )

putFood : Model -> Position -> (Model, Cmd Msg)
putFood model foodPosition =
    if isSnakePosition model.snake foodPosition then
        generateFood model
    else
        ( { model | food = foodPosition }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.status == GameOver then
        Sub.none
    else
        Sub.batch
            [ Time.every 250 MoveSnake
            , Events.onKeyDown keyDecoder
            ]

isSnakeTailPosition : SnakeTail -> Position -> Bool
isSnakeTailPosition tail cellPosition =
    Util.memberCallback (\tailPos -> isPositionEqual tailPos cellPosition) tail

isSnakePosition : Snake -> Position -> Bool
isSnakePosition snake cellPosition
    =  (isPositionEqual cellPosition snake.head)
    || (isSnakeTailPosition snake.tail cellPosition)  


renderCell : Model -> Int -> Int -> Html Msg
renderCell model rowIndex cellIndex =
    let
        cellPosition =
            { x = cellIndex
            , y = rowIndex
            }
    in
        if (isSnakePosition model.snake cellPosition) then
            snakeCell [] []
        else if isPositionEqual model.food cellPosition then
            foodCell [] []
        else
            emptyCell [] []

renderRow : Model -> Int -> Html Msg
renderRow model rowIndex =
    tr [] 
    (List.map (renderCell model rowIndex) (List.range 1 model.grid.columns))

renderGrid model =
    snakeTable [] 
    (List.map (renderRow model) (List.range 1 model.grid.rows))

gameView : Model -> Html Msg
gameView model =
    snakeContainer []
    [ renderGrid model
    ]

gameOverView : Html Msg
gameOverView =
    gameOverContainer []
    [ h1 [] [ text "Game over" ]
    ]

view : Model -> Html Msg
view model =
    if model.status == Running then
        gameView model
    else
        gameOverView

positionGenerator : Random.Generator Position
positionGenerator =
    Random.map2
        (\x y -> { x = x, y = y })
        (Random.int 1 10)
        (Random.int 1 10)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Msg
toDirection decodedKey =
    case decodedKey of
        "ArrowUp" ->
            ChangeDirection (Just Top)
        "ArrowDown" ->
            ChangeDirection (Just Bottom)
        "ArrowLeft" ->
            ChangeDirection (Just Left)
        "ArrowRight" ->
            ChangeDirection (Just Right)
        _ ->
            ChangeDirection Nothing

