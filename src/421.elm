module Main exposing (Model, Msg(..))

import Browser
import Css
import Css.Global
import Delay exposing (TimeUnit(..), after)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, src)
import Html.Styled.Events exposing (onClick)
import Random
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)



-- MAIN
-- TODO: add game doc
-- TODO: add history of actions
-- TODO: Responsive design
-- TODO: unit tests
-- TODO: integration tests


main =
    Browser.element
        { init = init
        , update = updateAppWith
        , subscriptions = subscriptions
        , view = view >> Html.Styled.toUnstyled
        }



-- MODEL


type alias Model =
    { dices : List Dice
    , rollsLeft : Int
    }


numDices : Int
numDices =
    3


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (List.repeat numDices (Dice Unlocked 1 0)) 3
    , Cmd.none
    )


type alias Dot =
    { coords : Coords, existsFor : List Face }


type alias Coords =
    { x : String, y : String }


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



-- UPDATE


type Msg
    = Roll
    | UpdateDices (Dice -> Dice -> Dice) (List Dice)
    | ShowFaces
    | Rebound
    | LockDice Int
    | NewTurn


updateAppWith : Msg -> Model -> ( Model, Cmd Msg )
updateAppWith msg model =
    case msg of
        Roll ->
            ( { model | rollsLeft = model.rollsLeft - 1 }
            , generateNewDicesThen (UpdateDices ifUnlocked)
            )

        UpdateDices onCondition withNewDices ->
            updateAppWith ShowFaces (updateDicesOf model onCondition withNewDices)

        ShowFaces ->
            ( model
            , if oneStillRolling model.dices then
                after 200 Millisecond Rebound

              else
                Cmd.none
            )

        Rebound ->
            ( model, generateNewDicesThen (UpdateDices ifRollingAndUnlocked) )

        LockDice withId ->
            ( Model (apply lockDice withId model.dices) model.rollsLeft
            , Cmd.none
            )

        NewTurn ->
            updateAppWith Roll (prepareForNewTurn model)


oneStillRolling : List Dice -> Bool
oneStillRolling dices =
    List.any (\dice -> dice.reboundsLeft > 0) dices


lockDice : Dice -> Dice
lockDice dice =
    case dice.status of
        Locked ->
            { dice | status = Unlocked }

        Unlocked ->
            { dice | status = Locked }


prepareForNewTurn : Model -> Model
prepareForNewTurn model =
    Model (List.map (\dice -> { dice | status = Unlocked }) model.dices) 3


turnHasEnded : Model -> Bool
turnHasEnded model =
    model.rollsLeft
        == 0
        || List.all (\dice -> dice.status == Locked) model.dices


ifRollingAndUnlocked : Dice -> Dice -> Dice
ifRollingAndUnlocked oldDice newDice =
    -- update old dice if rolling and unlocked
    if oldDice.reboundsLeft > 0 && oldDice.status == Unlocked then
        Dice oldDice.status newDice.face (oldDice.reboundsLeft - 1)

    else
        oldDice


ifUnlocked : Dice -> Dice -> Dice
ifUnlocked oldDice newDice =
    -- update old dice if it is unlocked
    case oldDice.status of
        Locked ->
            oldDice

        Unlocked ->
            newDice


generateNewDicesThen : (List Dice -> Msg) -> Cmd Msg
generateNewDicesThen message =
    -- Helper to generate new Dices and fire the desired message with the result
    Random.generate message
        (Random.list numDices
            (Random.map2 (Dice Unlocked) (Random.int 1 6) (Random.int 1 15))
        )


apply : (a -> a) -> Int -> List a -> List a
apply callable onIndex inList =
    List.indexedMap
        (\id ->
            \a ->
                if id == onIndex then
                    callable a

                else
                    a
        )
        inList


updateDicesOf : Model -> (Dice -> Dice -> Dice) -> List Dice -> Model
updateDicesOf model updateMethod newDices =
    { model | dices = List.map2 updateMethod model.dices newDices }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    mainLayout []
        [ rowLayout [] (drawDicesOf model)
        , rowLayout []
            [ p []
                [ Html.Styled.text
                    ("Rolls left: " ++ String.fromInt model.rollsLeft)
                ]
            , nextActionButtonFor model
            ]
        , Css.Global.global
            [ Css.Global.body [ Css.height (Css.pct 100) ]
            , Css.Global.html [ Css.height (Css.pct 100) ]
            ]
        ]


nextActionButtonFor : Model -> Html Msg
nextActionButtonFor model =
    if turnHasEnded model then
        button [ disabled (oneStillRolling model.dices), onClick NewTurn ]
            [ Html.Styled.text "New turn" ]

    else
        button [ disabled (oneStillRolling model.dices), onClick Roll ]
            [ Html.Styled.text "Roll" ]


drawDicesOf : Model -> List (Html Msg)
drawDicesOf model =
    -- Dices should only be lockable after the first throw
    List.indexedMap (drawDice (model.rollsLeft < 3)) model.dices


drawDice : Bool -> Int -> Dice -> Html Msg
drawDice lockable withId dice =
    svg
        ([ width "100%", height "100%", viewBox "0 0 120 120" ]
            ++ actionFor lockable dice withId
        )
        [ g
            (colorOf dice)
            (drawBorder ++ drawFace dice.face)
        ]


actionFor : Bool -> Dice -> Int -> List (Svg.Styled.Attribute Msg)
actionFor lockable dice withId =
    -- dice should only be lockable when it's enabled and they are still
    if lockable && dice.reboundsLeft == 0 then
        [ onClick (LockDice withId) ]

    else
        []


colorOf : Dice -> List (Svg.Styled.Attribute Msg)
colorOf dice =
    -- use dice color for strokes and fillings
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
        ([ fill "none", x "10", y "10", width "100", height "100", rx "12.5" ]
            ++ [ strokeWidth "1.5" ]
        )
        []
    ]


drawFace : Face -> List (Svg msg)
drawFace face =
    List.map drawDot (List.filter (dotExistsFor face) dots)


drawDot : Dot -> Svg msg
drawDot { coords } =
    circle [ cx coords.x, cy coords.y, r "13" ] []


mainLayout : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
mainLayout =
    Html.Styled.styled div
        [ Css.maxWidth (Css.ch 70)
        , Css.padding (Css.ch 2)
        , Css.margin Css.auto
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        , Css.height (Css.pct 100)
        ]


rowLayout : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
rowLayout =
    Html.Styled.styled div
        [ Css.displayFlex
        , Css.justifyContent Css.spaceAround
        , Css.width (Css.pct 100)
        , Css.alignItems Css.center
        ]
