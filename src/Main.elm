module Main exposing (..)

import Browser
import Debug
import Html.Styled exposing (Html, button, div, text, tr, td, h1, toUnstyled, text)
import Components exposing (snakeTable, emptyCell, snakeContainer, gameOverContainer, snakeCell)
import Html.Events exposing (onClick)
import List
import Time
import Json.Decode as Decode
import Browser.Events as Events
import Position exposing (Position, isPositionEqual)

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

type GameStatus = Running | Ended

type alias Snake =
    { head : Position
    , moveDirection: SnakeMoveDirection
    }

type alias Model =
    { grid : Grid
    , snake : Snake
    , status: GameStatus
    }

initGrid : Grid
initGrid =
    { columns = 15
    , rows = 15
    }

initSnake : Snake
initSnake =
    { head = initSnakeHead
    , moveDirection = Right
    }

initSnakeHead : Position
initSnakeHead =
    { x = initGrid.columns // 2
    , y = initGrid.rows // 2
    }

initData : Model
initData =
    { grid = initGrid
    , snake = initSnake
    , status = Running
    }

init: () -> (Model, Cmd Msg)
init _ =
    ( initData
    , Cmd.none
    )

isPositionOutside : Position -> Grid -> Bool
isPositionOutside position grid 
    =  position.x < 1
    || position.y < 1
    || position.x > grid.columns
    || position.y > grid.rows

moveSnake : Model -> (Model, Cmd Msg)
moveSnake model =
    let
        updatedModel = { model | snake = (changeSnake model.snake) }
    in
        if isPositionOutside updatedModel.snake.head model.grid then
            ( { model | status = Ended }
            , Cmd.none
            )
        else
            ( updatedModel
            , Cmd.none
            )

changeSnake : Snake -> Snake
changeSnake snake =
    let
        direction = snake.moveDirection
        x = snake.head.x
        y = snake.head.y
    in
        case direction of
            Top -> { snake | head = {x = x, y = y - 1}}
            Bottom -> { snake | head = {x = x, y = y + 1}}
            Right -> { snake | head = {x = x + 1, y = y}}
            Left -> { snake | head = {x = x - 1, y = y}}

changeSnakeDirection : Model -> Maybe SnakeMoveDirection -> (Model, Cmd Msg)
changeSnakeDirection model maybeMove =
    let 
        updatedModel =
            case maybeMove of
                Just move ->
                    let snake = model.snake
                        updatedSnake = { snake | moveDirection = move }
                    in { model | snake = updatedSnake }
                Nothing -> model
    in
        ( updatedModel
        , Cmd.none
        )
        
type Msg 
    = MoveSnake Time.Posix
    | ChangeDirection (Maybe SnakeMoveDirection)
    | GameOver

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MoveSnake _ ->
            moveSnake model
        ChangeDirection maybeMove ->
            changeSnakeDirection model maybeMove
        GameOver ->
            ( { model | status = Ended }
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 500 MoveSnake
        , Events.onKeyDown keyDecoder
        ]
    


renderCell : Model -> Int -> Int -> Html Msg
renderCell model rowIndex cellIndex =
    let
        cellPosition =
            { x = cellIndex
            , y = rowIndex
            }
        snakeHeadPosition =
            model.snake.head
    in
        if (isPositionEqual cellPosition snakeHeadPosition) then
            snakeCell [] []
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

