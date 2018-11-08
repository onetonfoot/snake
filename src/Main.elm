module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Color
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, id, max, min, style, type_, value)
import Html.Events exposing (..)
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List
import List.Extra exposing (allDifferent)
import Maybe
import Random exposing (Generator, int, pair)
import Random.Extra exposing (sample)
import Time
import TypedSvg exposing (circle, g, rect, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width, x, y)
import TypedSvg.Types exposing (Fill(..), pc, px)



--MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--TYPES & MESSAGES


type alias Model =
    { apple : Maybe Block
    , snake : Snake
    , direction : Direction
    , width : Float
    , height : Float
    , score : Int
    , level : Int
    , n : Float
    , isDead : Bool
    , paused : Bool
    }


type Msg
    = NewApple
    | RandomApple (Maybe Block)
    | HandleKeyboardEvent KeyboardEvent
    | Tick Time.Posix
    | LevelChange String
    | NoOp


type alias Block =
    { x : Float
    , y : Float
    }


type alias Snake =
    List Block


type Direction
    = Up
    | Down
    | Left
    | Right



--INIT


allBlocks : List Block
allBlocks =
    List.map
        (\point -> Block (toFloat <| Tuple.first point) (toFloat <| Tuple.second point))
    <|
        cartesian (List.range 1 (round nCells)) (List.range 1 (round nCells))


nCells : Float
nCells =
    30


initSnake : Snake
initSnake =
    let
        middle =
            toFloat (ceiling (nCells / 2))
    in
    [ Block middle middle
    , Block (middle - 1) middle
    , Block (middle - 2) middle
    ]


initModel : Model
initModel =
    { apple = Nothing
    , direction = Right
    , width = 600.0
    , height = 600.0
    , score = 0
    , level = 10
    , n = nCells
    , snake = initSnake
    , isDead = False
    , paused = True
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel
    , Cmd.none
    )



-- GENERATORS


randomApple : Model -> Generator (Maybe Block)
randomApple model =
    let
        filteredBlocks =
            List.filter (\block -> not (List.member block model.snake)) allBlocks
    in
    sample filteredBlocks



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewApple ->
            ( model
            , Random.generate RandomApple (randomApple model)
            )

        RandomApple apple ->
            ( { model | apple = apple }, Cmd.none )

        HandleKeyboardEvent event ->
            ( handleKeyboardEvent model event, Cmd.none )

        LevelChange value ->
            ( updateLevel model value, Cmd.none )

        Tick _ ->
            updateGame model

        NoOp ->
            ( model, Cmd.none )


handleKeyboardEvent : Model -> KeyboardEvent -> Model
handleKeyboardEvent model event =
    case event.key of
        Nothing ->
            model

        Just string ->
            case string of
                "ArrowUp" ->
                    if model.direction == Down then
                        model

                    else
                        { model | direction = Up }

                "ArrowDown" ->
                    if model.direction == Up then
                        model

                    else
                        { model | direction = Down }

                "ArrowLeft" ->
                    if model.direction == Right then
                        model

                    else
                        { model | direction = Left }

                "ArrowRight" ->
                    if model.direction == Left then
                        model

                    else
                        { model | direction = Right }

                " " ->
                    if model.isDead then
                        { initModel | paused = False }

                    else
                        { model | paused = not model.paused }

                _ ->
                    model


updateSnakeDirection : Snake -> Direction -> Snake
updateSnakeDirection snake direction =
    case snake of
        front :: back ->
            case direction of
                Up ->
                    let
                        newFront =
                            { front | y = front.y - 1 }
                    in
                    newFront :: List.take (List.length snake - 1) snake

                Down ->
                    let
                        newFront =
                            { front | y = front.y + 1 }
                    in
                    newFront :: List.take (List.length snake - 1) snake

                Left ->
                    let
                        newFront =
                            { front | x = front.x - 1 }
                    in
                    newFront :: List.take (List.length snake - 1) snake

                Right ->
                    let
                        newFront =
                            { front | x = front.x + 1 }
                    in
                    newFront :: List.take (List.length snake - 1) snake

        _ ->
            snake


tryEatApple : Model -> Model
tryEatApple model =
    let
        newSnake =
            updateSnakeDirection model.snake model.direction
    in
    case model.isDead of
        True ->
            model

        False ->
            case model.apple of
                Nothing ->
                    { model | snake = newSnake }

                Just apple ->
                    if List.member apple newSnake then
                        let
                            end =
                                List.drop (List.length model.snake - 1) model.snake

                            happySnake =
                                List.append newSnake end
                        in
                        { model | snake = happySnake, apple = Nothing, score = model.score + 1 }

                    else
                        { model | snake = newSnake }


checkIfDead : Model -> Model
checkIfDead model =
    let
        outOfBounds =
            case model.snake of
                front :: back ->
                    (front.x == 1 && model.direction == Left)
                        || (front.y == 1 && model.direction == Up)
                        || (front.x == model.n && model.direction == Right)
                        || (front.y == model.n && model.direction == Down)

                _ ->
                    True

        ateSelf =
            not (allDifferent (List.map (\block -> ( block.x, block.y )) model.snake))

        isDead =
            outOfBounds || ateSelf
    in
    { model | isDead = isDead, paused = isDead }


updateGame : Model -> ( Model, Cmd Msg )
updateGame model =
    let
        newModel =
            model
                |> checkIfDead
                |> tryEatApple

        cmd =
            case model.apple of
                Nothing ->
                    Random.generate RandomApple (randomApple model)

                Just _ ->
                    Cmd.none
    in
    ( newModel, cmd )


updateLevel : Model -> String -> Model
updateLevel model value =
    case String.toInt value of
        Nothing ->
            model

        Just i ->
            { model | level = i }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyPressedSub =
            onKeyDown (Decode.map HandleKeyboardEvent decodeKeyboardEvent)

        timeSub =
            calculateSpeed model
    in
    if model.paused then
        keyPressedSub

    else
        Sub.batch [ keyPressedSub, timeSub ]


calculateSpeed : Model -> Sub Msg
calculateSpeed model =
    let
        n =
            (sqrt <| sqrt <| toFloat <| model.score + 1) * (toFloat <| model.level)
    in
    -- Time.every (toFloat (1000 // model.level)) Tick
    Time.every (1000 / n) Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ overlay model
        , game
            model
        ]


overlay : Model -> Html Msg
overlay model =
    if model.paused || model.isDead then
        div [ id "overlay", style "background-color" "rgba(0, 0, 0, 0.8)" ]
            [ h2 []
                [ text
                    "Press space to play and pause"
                , br [] []
                , text "Use arrow keys to move."
                ]
            ]

    else
        div
            [ id "overlay", style "background-color" "rgba(0, 0, 0, 0.0)" ]
            []



-- game : Model -> Html Msg
-- game model =
--     div []
--         [ h1 [] [ text <| (++) "Score: " <| String.fromInt model.score ]
--         , input [ type_ "range", min "1", max "10", value <| String.fromInt model.level, onChange LevelChange ] []
--         , div [ id "overlay" ]
--             []
--         ]


game : Model -> Html Msg
game model =
    svg
        [ width (px model.width)
        , height (px model.height)
        , id "game"
        ]
        [ rect [ width (px model.width), height (px model.height), fill <| Fill Color.lightGray ]
            []
        , renderApple model
        , renderSnake model
        ]


renderSnake : Model -> Html Msg
renderSnake model =
    g []
        (List.map
            (\block -> postitionBlock model Color.green block)
            model.snake
        )


renderApple : Model -> Html Msg
renderApple model =
    let
        positionApple =
            postitionBlock model Color.red

        appleBlock =
            case model.apple of
                Nothing ->
                    rect [] []

                Just apple ->
                    positionApple apple
    in
    appleBlock


postitionBlock : Model -> Color.Color -> Block -> Html Msg
postitionBlock model color block =
    let
        stepX =
            model.width / model.n

        stepY =
            model.height / model.n

        x1 =
            px <| stepX * (block.x - 1)

        y1 =
            px <| stepY * (block.y - 1)
    in
    rect
        [ width (px stepX)
        , height (px stepY)
        , x x1
        , y y1
        , fill <| Fill color
        ]
        []



-- HELPER FUNCTIONS


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" <| Decode.map handler <| Decode.at [ "target", "value" ] Decode.string
