module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Delay exposing (TimeUnit(..), after)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (disabled, src)
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
    , numRolls : Int
    }


type alias Dice =
    { status : Status
    , face : Face
    , reboundsLeft : Int
    }


type Status
    = Locked
    | Unlocked


type alias Face =
    Int


type alias Dot =
    { coords : Coords, existsFor : List Face }


type alias Coords =
    { x : String, y : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (List.repeat numDices (Dice Locked 1 0)) 3
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
    | ShowFaces
    | Rebound
    | ToggleDice Int
    | Update (Dice -> Dice -> Dice) (List Dice)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( prepareModelForRoll model
            , generateNewDicesThen (Update ifUnlocked)
            )

        ShowFaces ->
            ( model
            , if oneStillRolling model.dices then
                after 200 Millisecond Rebound

              else
                Cmd.none
            )

        Rebound ->
            ( model, generateNewDicesThen (Update ifRolling) )

        ToggleDice dice_id ->
            ( Model (applyOn toggleDice dice_id model.dices) model.numRolls
            , Cmd.none
            )

        Update method newDices ->
            update ShowFaces (updateDices model method newDices)


prepareModelForRoll : Model -> Model
prepareModelForRoll model =
    if roundHasEnded model then
        Model (List.repeat numDices (Dice Unlocked 1 0)) 1

    else
        { model | numRolls = model.numRolls + 1 }


roundHasEnded : Model -> Bool
roundHasEnded model =
    model.numRolls == 3 || List.all (\dice -> dice.status == Locked) model.dices


toggleDice : Dice -> Dice
toggleDice dice =
    case dice.status of
        Locked ->
            { dice | status = Unlocked }

        Unlocked ->
            { dice | status = Locked }


applyOn : (a -> a) -> Int -> List a -> List a
applyOn callable index list =
    List.indexedMap
        (\id ->
            \a ->
                if id == index then
                    callable a

                else
                    a
        )
        list


generateNewDicesThen : (List Dice -> Msg) -> Cmd Msg
generateNewDicesThen message =
    Random.generate message
        (Random.list numDices
            (Random.map2 (Dice Unlocked) (Random.int 1 6) (Random.int 1 15))
        )


oneStillRolling : List Dice -> Bool
oneStillRolling dices =
    List.any (\dice -> dice.reboundsLeft > 0) dices


updateDices : Model -> (Dice -> Dice -> Dice) -> List Dice -> Model
updateDices model updateMethod newDices =
    { model | dices = List.map2 updateMethod model.dices newDices }


ifRolling : Dice -> Dice -> Dice
ifRolling oldDice newDice =
    if oldDice.reboundsLeft > 0 && oldDice.status == Unlocked then
        Dice oldDice.status newDice.face (oldDice.reboundsLeft - 1)

    else
        oldDice


ifUnlocked : Dice -> Dice -> Dice
ifUnlocked oldDice newDice =
    case oldDice.status of
        Locked ->
            oldDice

        Unlocked ->
            newDice



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ p [] [ Html.text ("Rolls: " ++ String.fromInt model.numRolls) ]
            , button [ disabled (oneStillRolling model.dices), onClick Roll ]
                [ Html.text "Reroll" ]
            ]
        , div [] (List.indexedMap drawDice model.dices)
        ]


drawDice : Int -> Dice -> Html Msg
drawDice index dice =
    svg
        ([ width "120"
         , height "120"
         , viewBox "0 0 120 120"
         ]
            ++ toggleOnClickIfStill index dice
        )
        [ g (setColor dice) (drawBorder ++ drawFace dice.face) ]


toggleOnClickIfStill : Int -> Dice -> List (Svg.Attribute Msg)
toggleOnClickIfStill index dice =
    if dice.reboundsLeft == 0 then
        [ onClick (ToggleDice index) ]

    else
        []


setColor : Dice -> List (Svg.Attribute Msg)
setColor dice =
    List.map (\x -> x (getColor dice)) [ stroke, fill ]


getColor : Dice -> String
getColor dice =
    if dice.reboundsLeft == 0 then
        case dice.status of
            Locked ->
                "green"

            Unlocked ->
                "black"

    else
        "gray"


drawBorder : List (Svg msg)
drawBorder =
    [ rect
        ([ fill "none", x "10", y "10", width "100" ]
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
