module Main exposing (Model, Msg(..))

import Browser
import Css
import Css.Animations
import Css.Global
import Css.Transitions exposing (easeInOut, transition)
import Delay exposing (TimeUnit(..), after)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, src)
import Html.Styled.Events exposing (onClick)
import Random
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)



-- MAIN
-- TODO: configure cloudflare hosting
-- TODO: add game doc
-- TODO: add history of actions
-- TODO: manage User & party
-- TODO: unit tests
-- TODO: integration tests
-- TODO: add favicon
-- TODO: change cloudflare keys
-- TODO: set pointer on clickable dices
-- TODO: gray-out dices that are not clickable
-- TODO: reduce favicon dice size https://www.browserling.com/tools/image-to-base64


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
    [ Dot (Coords "32" "32") [ 2, 3, 4, 5, 6 ]
    , Dot (Coords "60" "32") [ 6 ]
    , Dot (Coords "88" "32") [ 4, 5, 6 ]
    , Dot (Coords "60" "60") [ 1, 3, 5 ]
    , Dot (Coords "32" "88") [ 4, 5, 6 ]
    , Dot (Coords "60" "88") [ 6 ]
    , Dot (Coords "88" "88") [ 2, 3, 4, 5, 6 ]
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
        ([ rowLayout [] (drawDicesOf model)
         , rowLayout []
            [ p []
                [ Html.Styled.text
                    ("Rolls left: " ++ String.fromInt model.rollsLeft)
                ]
            , nextActionButtonFor model
            ]
         ]
            ++ globalStyles
        )


nextActionButtonFor : Model -> Html Msg
nextActionButtonFor model =
    if turnHasEnded model then
        button [ disabled (oneStillRolling model.dices), onClick NewTurn ]
            [ Html.Styled.text "New turn" ]

    else
        rerollButton (oneStillRolling model.dices)


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
            (colorOf dice ++ [ strokeWidth "1.5" ])
            (drawBorder ++ drawFace dice.face "13")
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
        [ fill "none", x "10", y "10", width "100", height "100", rx "12.5" ]
        []
    ]


drawFace : Face -> String -> List (Svg msg)
drawFace face radius =
    List.map (drawDot radius) (List.filter (dotExistsFor face) dots)


drawDot : String -> Dot -> Svg msg
drawDot radius { coords } =
    circle
        [ cx coords.x
        , cy coords.y
        , r radius
        ]
        []


mainLayout : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
mainLayout =
    Html.Styled.styled div mainLayoutStyles


rowLayout : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
rowLayout =
    Html.Styled.styled div rowStyles


rerollButton : Bool -> Html Msg
rerollButton inactive =
    if not inactive then
        Html.Styled.styled button
            activeRollingButtonStyles
            [ disabled False, onClick Roll ]
            [ rollingIcon ]

    else
        Html.Styled.styled button
            inactiveRollingButtonStyles
            [ disabled True, onClick Roll ]
            [ rollingIcon ]


rollingIcon : Html Msg
rollingIcon =
    Html.Styled.styled button
        activeRollingButtonStyles
        []
        [ svg
            [ width "32"
            , height "32"
            , viewBox "-20 -20 160 160"
            , fill "currentColor"
            , fillRule "nonzero"
            , stroke "currentColor"
            , strokeWidth "11"
            , transform "rotate(-7)"
            ]
            (drawBorder
                ++ drawFace 5 "3"
                ++ [ Svg.Styled.path
                        [ d
                            ("M 95 -10 a 30 30 0 0 1 35 35"
                                ++ "M -10 95 a 30 30 0 0 0 35 35"
                            )
                        , fill "none"
                        ]
                        []
                   ]
            )
        ]



-- Styles


globalStyles : List (Html Msg)
globalStyles =
    [ Css.Global.global
        [ Css.Global.body [ Css.height (Css.pct 100) ]
        , Css.Global.html [ Css.height (Css.pct 100) ]
        ]
    ]


mainLayoutStyles : List Css.Style
mainLayoutStyles =
    [ Css.maxWidth (Css.ch 70)
    , Css.padding (Css.ch 2)
    , Css.margin Css.auto
    , Css.displayFlex
    , Css.flexDirection Css.column
    , Css.alignItems Css.center
    , Css.justifyContent Css.center
    , Css.height (Css.pct 100)
    ]


inactiveRollingButtonStyles : List Css.Style
inactiveRollingButtonStyles =
    baseRollingButtonStyles
        ++ [ Css.backgroundColor (Css.hex "#66c2ff")
           ]


activeRollingButtonStyles : List Css.Style
activeRollingButtonStyles =
    baseRollingButtonStyles
        ++ [ Css.backgroundColor (Css.hex "#0099FF")
           , Css.active
                [ Css.animationName
                    (Css.Animations.keyframes
                        [ ( 0
                          , [ Css.Animations.property
                                "transform"
                                "rotate(0deg)"
                            ]
                          )
                        , ( 100
                          , [ Css.Animations.property
                                "transform"
                                "rotate(360deg)"
                            ]
                          )
                        ]
                    )
                , Css.animationDuration (Css.ms 500)
                ]
           , Css.hover
                [ Css.transform (Css.scale 1.1) ]
           ]


baseRollingButtonStyles : List Css.Style
baseRollingButtonStyles =
    [ Css.color (Css.hex "#FFF")
    , Css.border (Css.px 0)
    , Css.cursor Css.pointer
    , Css.padding (Css.px 0)
    , Css.paddingLeft (Css.px 0)
    , Css.paddingRight (Css.px 0)
    , Css.borderRadius (Css.pct 100)
    , Css.width (Css.rem 4)
    , Css.height (Css.rem 4)
    , Css.display Css.inlineFlex
    , Css.justifyContent Css.center
    , Css.alignItems Css.center
    , Css.outline Css.none
    , Css.focus
        [ Css.boxShadow5
            (Css.px 0)
            (Css.px 0)
            (Css.px 0)
            (Css.rem 0.25)
            (Css.hex "#c4e7ff")
        ]
    ]


rowStyles : List Css.Style
rowStyles =
    [ Css.displayFlex
    , Css.justifyContent Css.spaceAround
    , Css.width (Css.pct 100)
    , Css.alignItems Css.center
    ]
