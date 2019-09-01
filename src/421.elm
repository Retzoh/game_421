module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Delay exposing (TimeUnit(..), after)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dices : List Dice
    }


type alias Dice =
    { face : Face
    , reboundsLeft : Int
    }


type alias Face =
    Int


type alias Dot =
    { coords : Coords, existsFor : List Face }


type alias Coords =
    { x : String, y : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
    , Cmd.none
    )


dots : List Dot
dots =
    [ Dot (Coords "30" "30") [ 2, 3, 4, 5, 6 ]
    , Dot (Coords "60" "30") [ 6 ]
    , Dot (Coords "90" "30") [ 4, 5, 6 ]
    , Dot (Coords "60" "60") [ 1, 3, 5 ]
    , Dot (Coords "30" "90") [ 4, 5, 6 ]
    , Dot (Coords "60" "90") [ 6 ]
    , Dot (Coords "90" "90") [ 2, 3, 4, 5, 6 ]
    ]


dotExistsFor : Face -> Dot -> Bool
dotExistsFor face dot =
    List.member face dot.existsFor


numDices : Int
numDices =
    3



-- UPDATE


type Msg
    = Roll
    | ShowFaces (List Dice)
    | Rebound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, generateNewFacesThen ShowFaces )

        ShowFaces dices ->
            ( Model dices
            , if oneStillRolling dices then
                after 200 Millisecond Rebound

              else
                Cmd.none
            )

        Rebound ->
            ( model, generateNewFacesThen (updateRollingDices model) )


generateNewFacesThen : (List Dice -> Msg) -> Cmd Msg
generateNewFacesThen message =
    Random.generate message
        (Random.list numDices
            (Random.map2 Dice (Random.int 1 6) (Random.int 1 15))
        )


oneStillRolling : List Dice -> Bool
oneStillRolling dices =
    List.any (\dice -> dice.reboundsLeft > 0) dices


updateRollingDices : Model -> List Dice -> Msg
updateRollingDices model newDices =
    ShowFaces (List.map2 updateDiceIfRolling model.dices newDices)


updateDiceIfRolling : Dice -> Dice -> Dice
updateDiceIfRolling oldDice newDice =
    if oldDice.reboundsLeft > 0 then
        Dice newDice.face (oldDice.reboundsLeft - 1)

    else
        oldDice



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ button [ onClick Roll ] [ Html.text "Roll" ] ]
        , div [] (List.map drawDice model.dices)
        ]


drawDice : Dice -> Html msg
drawDice dice =
    svg
        [ width "120", height "120", viewBox "0 0 120 120" ]
        (drawBorder ++ drawFace dice.face)


drawBorder : List (Svg msg)
drawBorder =
    [ rect
        ([ fill "none", stroke "black", x "10", y "10", width "100" ]
            ++ [ height "100", rx "12.5", strokeWidth "1.5" ]
        )
        []
    ]


drawFace : Face -> List (Svg msg)
drawFace face =
    List.map drawDot (List.filter (dotExistsFor face) dots)


drawDot : Dot -> Svg msg
drawDot { coords } =
    circle [ cx coords.x, cy coords.y, Svg.Attributes.r "13" ] []
