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
-- TODO: add history of actions
-- TODO: manage User & party
-- TODO: unit tests
-- TODO: integration tests
-- TODO: add favicon
-- TODO: change cloudflare keys
-- TODO: roll dice faces according to the real geometry
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
    , showInstructions : Bool
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
    ( Model (List.repeat numDices (Dice Unlocked 1 0)) 3 False
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
    | ToggleInstructions


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
            ( { model | dices = apply lockDice withId model.dices }
            , Cmd.none
            )

        NewTurn ->
            ( prepareForNewTurn model, Cmd.none )

        ToggleInstructions ->
            ( { model | showInstructions = not model.showInstructions }
            , Cmd.none
            )


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
    Model (List.map (\dice -> { dice | status = Unlocked }) model.dices)
        3
        model.showInstructions


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
    mainLayout
        (titleAndNav
            ++ contentLayout
                (rowLayout (drawDicesOf model)
                    ++ rowLayout
                        (rollsLeftIndication model.rollsLeft
                            ++ nextActionButtonFor model
                        )
                    ++ (case model.showInstructions of
                            True ->
                                instructions

                            False ->
                                []
                       )
                    ++ globalStyles
                )
        )


instructions : List (Html Msg)
instructions =
    [ Html.Styled.styled div
        instructionStyles
        []
        ([ Html.Styled.h3 [] [ Html.Styled.text "Instructions" ]
         , paragraph
            ("You have up to three rolls to get the best dice combination. "
                ++ "Click on a dice to lock or unlock it."
            )
         , paragraph
            ("Here is the list of the combinations, in decreasing order of"
                ++ " importance:"
            )
         , combinationTable
         , paragraph
            ("Between two combinations of a same category, the highest"
                ++ " number wins. For example 654 wins over 321 and 532 wins "
                ++ "over 221"
            )
         ]
            ++ rowLayout
                [ Html.Styled.styled button
                    mainButtonStyles
                    [ onClick ToggleInstructions ]
                    [ Html.Styled.text "Got it !" ]
                ]
        )
    ]


combinationTable : Html Msg
combinationTable =
    Html.Styled.styled table
        tableStyles
        []
        ([ combinationTableHeaders ]
            ++ List.map combinationTableRow
                [ ( "421", "8" )
                , ( "111", "7" )
                , ( "666, 116", "6" )
                , ( "555, 115", "5" )
                , ( "444, 114", "4" )
                , ( "333, 113", "3" )
                , ( "222, 112", "2" )
                , ( "654, 543, 432, 321", "2" )
                , ( "Others", "1" )
                ]
        )


combinationTableHeaders : Html Msg
combinationTableHeaders =
    tr []
        [ Html.Styled.styled th
            [ Css.width (Css.pct 50) ]
            []
            [ Html.Styled.text "Combination"
            ]
        , Html.Styled.styled th
            [ Css.width (Css.pct 50) ]
            []
            [ Html.Styled.text "Points"
            ]
        ]


combinationTableRow : ( String, String ) -> Html Msg
combinationTableRow ( combination, score ) =
    tr []
        [ td [] [ Html.Styled.text combination ]
        , td [] [ Html.Styled.text score ]
        ]


titleAndNav : List (Html Msg)
titleAndNav =
    titleAndNavLayout
        [ h1 [] [ Html.Styled.text "421 Game" ]
        , Html.Styled.styled button
            (baseActiveButtonStyles 2.5)
            [ onClick ToggleInstructions ]
            [ Html.Styled.text "i" ]
        ]


rollsLeftIndication : Int -> List (Html Msg)
rollsLeftIndication rollsLeft =
    [ paragraph ("Rolls left: " ++ String.fromInt rollsLeft) ]


nextActionButtonFor : Model -> List (Html Msg)
nextActionButtonFor model =
    if turnHasEnded model then
        nextPlayerButton (oneStillRolling model.dices)

    else
        rerollButton (oneStillRolling model.dices)


drawDicesOf : Model -> List (Html Msg)
drawDicesOf model =
    -- Dices should only be lockable after the first throw
    List.indexedMap (drawDice (model.rollsLeft < 3)) model.dices


withBaseDotRadius =
    "13"


drawDice : Bool -> Int -> Dice -> Html Msg
drawDice lockable withId dice =
    Svg.Styled.styled svg
        (styleOf lockable dice)
        ([ width "100%", height "100%", viewBox "0 0 120 120" ]
            ++ actionFor lockable dice withId
        )
        [ g
            (colorOf lockable dice ++ [ strokeWidth "1.5" ])
            (drawBorder ++ drawFace dice.face withBaseDotRadius)
        ]


actionFor : Bool -> Dice -> Int -> List (Svg.Styled.Attribute Msg)
actionFor lockable dice withId =
    -- dice should only be lockable when it's enabled and they are still
    if lockable && dice.reboundsLeft == 0 then
        [ onClick (LockDice withId) ]

    else
        []


colorOf : Bool -> Dice -> List (Svg.Styled.Attribute Msg)
colorOf lockable dice =
    -- use dice color for strokes and fillings
    List.map (\x -> x (getColorOf lockable dice)) [ stroke, fill ]


getColorOf : Bool -> Dice -> String
getColorOf lockable dice =
    if lockable && dice.reboundsLeft == 0 then
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
drawFace face withRadius =
    List.map (drawDot withRadius) (List.filter (dotExistsFor face) dots)


drawDot : String -> Dot -> Svg msg
drawDot withRadius { coords } =
    circle [ cx coords.x, cy coords.y, r withRadius ] []


rerollButton : Bool -> List (Html Msg)
rerollButton inactive =
    [ Html.Styled.styled button
        (case inactive of
            True ->
                inactiveRollingButtonStyles

            False ->
                activeRollingButtonStyles
        )
        [ disabled inactive, onClick Roll ]
        rerollIcon
    ]


rerollIcon : List (Html Msg)
rerollIcon =
    [ svg
        [ width "32"
        , height "42"
        , viewBox "-20 -20 160 160"
        , fill "currentColor"
        , fillRule "nonzero"
        , stroke "currentColor"
        , strokeWidth "11"
        , transform "rotate(-7)"
        ]
        (drawBorder ++ drawFace 5 "3" ++ drawMovement)
    ]


drawMovement : List (Html Msg)
drawMovement =
    [ Svg.Styled.path
        [ d
            ("M 95 -10 a 30 30 0 0 1 35 35"
                ++ "M -10 95 a 30 30 0 0 0 35 35"
            )
        , fill "none"
        , strokeLinecap "round"
        ]
        []
    ]


nextPlayerButton : Bool -> List (Html Msg)
nextPlayerButton isInactive =
    [ Html.Styled.styled button
        (case isInactive of
            True ->
                inactiveRollingButtonStyles

            False ->
                activeRollingButtonStyles
        )
        [ disabled isInactive, onClick NewTurn ]
        (nextPlayerIcon isInactive)
    ]


type Position
    = FrontActive
    | FrontInactive
    | Back


type OnSide
    = Left
    | Right


nextPlayerIcon : Bool -> List (Html Msg)
nextPlayerIcon isInactive =
    [ svg
        [ width "52"
        , height "42"
        , viewBox "-70 -70 140 140"
        , fill "currentColor"
        , fillRule "nonzero"
        , stroke "currentColor"
        , strokeWidth "5"
        ]
        [ g [ strokeLinejoin "round", strokeLinecap "round" ]
            (drawPlayer Back
                ++ drawPlayer
                    (case isInactive of
                        True ->
                            FrontInactive

                        False ->
                            FrontActive
                    )
                ++ drawArrow Right
                ++ drawArrow Left
            )
        ]
    ]


drawPlayer : Position -> List (Svg Msg)
drawPlayer position =
    [ g
        (case position of
            FrontActive ->
                [ stroke "#0099FF" ]

            FrontInactive ->
                [ stroke "#66c2ff" ]

            Back ->
                [ fill "none", transform "translate(0,-20)" ]
        )
        [ polygon [ points "0,5 30,60 -30,60" ] []
        , circle [ cx "0", cy "-10", r "20" ] []
        ]
    ]


drawArrow : OnSide -> List (Svg Msg)
drawArrow onSide =
    [ g [ transform "translate(0,0)" ]
        [ g
            (case onSide of
                Left ->
                    [ transform "rotate(180)" ]

                Right ->
                    []
            )
            [ Svg.Styled.path [ d "M 55 28 a 29 29 0 0 0 5 -50", fill "none" ] []
            , g [ transform "translate(60,-22),rotate(-45)" ]
                [ polygon [ points "0,-10 10,0 0,-10 -10,0" ] [] ]
            ]
        ]
    ]


mainLayout : List (Html msg) -> Html msg
mainLayout =
    Html.Styled.styled div mainLayoutStyles []


contentLayout : List (Html Msg) -> List (Html Msg)
contentLayout content =
    [ Html.Styled.styled div contentLayoutStyles [] content ]


rowLayout : List (Html msg) -> List (Html msg)
rowLayout content =
    [ Html.Styled.styled div rowStyles [] content ]


titleAndNavLayout : List (Html msg) -> List (Html msg)
titleAndNavLayout content =
    [ Html.Styled.styled div titleAndNavRowStyles [] content ]


paragraph : String -> Html Msg
paragraph text =
    Html.Styled.p [] [ Html.Styled.text text ]



-- STYLES


globalStyles : List (Html Msg)
globalStyles =
    [ Css.Global.global
        [ Css.Global.body
            [ Css.height (Css.pct 100)
            , Css.fontFamilies [ "Montserrat" ]
            , Css.textAlign Css.justify
            , Css.margin (Css.px 0)
            ]
        , Css.Global.html
            [ Css.height (Css.pct 100) ]
        ]
    ]


mainLayoutStyles : List Css.Style
mainLayoutStyles =
    [ Css.maxWidth (Css.ch 70)
    , Css.paddingLeft (Css.ch 2)
    , Css.paddingRight (Css.ch 2)
    , Css.margin Css.auto
    , Css.position Css.relative
    , Css.height (Css.pct 100)
    , Css.displayFlex
    , Css.flexDirection Css.column
    , Css.alignItems Css.center
    , Css.justifyContent Css.stretch
    ]


contentLayoutStyles : List Css.Style
contentLayoutStyles =
    [ Css.displayFlex
    , Css.flexDirection Css.column
    , Css.alignItems Css.center
    , Css.justifyContent Css.center
    , Css.height Css.inherit
    ]


inactiveRollingButtonStyles : List Css.Style
inactiveRollingButtonStyles =
    baseIconButtonStyles 4
        ++ [ Css.backgroundColor palette.light ]


activeRollingButtonStyles : List Css.Style
activeRollingButtonStyles =
    baseActiveButtonStyles 4
        ++ [ Css.active
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
                                "rotate(-360deg)"
                            ]
                          )
                        ]
                    )
                , Css.animationDuration (Css.ms 500)
                ]
           , Css.hover
                [ Css.transform (Css.scale 1.1) ]
           ]


baseActiveButtonStyles : Float -> List Css.Style
baseActiveButtonStyles size =
    baseIconButtonStyles size
        ++ [ Css.cursor Css.pointer ]


baseIconButtonStyles : Float -> List Css.Style
baseIconButtonStyles size =
    baseButtonStyles
        ++ [ Css.borderRadius (Css.pct 100)
           , Css.width (Css.rem size)
           , Css.height (Css.rem size)
           , Css.fontSize (Css.px 25)
           ]


baseButtonStyles : List Css.Style
baseButtonStyles =
    [ Css.backgroundColor palette.medium
    , Css.fontSize (Css.rem 1)
    , Css.color palette.white
    , Css.fontFamilies [ "Montserrat" ]
    , Css.fontWeight (Css.int 500)
    , Css.border (Css.px 0)
    , Css.padding (Css.px 0)
    , Css.paddingLeft (Css.px 0)
    , Css.paddingRight (Css.px 0)
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
            palette.ultraLight
        ]
    , baseShadow
    ]


instructionStyles : List Css.Style
instructionStyles =
    [ Css.position Css.absolute
    , Css.backgroundColor palette.white
    , Css.borderRadius (Css.ch 1)
    , Css.padding (Css.ch 2)
    , Css.width (Css.pct 85)
    , baseShadow
    ]


mainButtonStyles : List Css.Style
mainButtonStyles =
    baseButtonStyles
        ++ [ Css.padding (Css.ch 1)
           , Css.borderRadius (Css.px 3)
           , Css.hover [ Css.cursor Css.pointer ]
           ]


rowStyles : List Css.Style
rowStyles =
    baseRowStyles ++ [ Css.justifyContent Css.spaceAround ]


titleAndNavRowStyles : List Css.Style
titleAndNavRowStyles =
    baseRowStyles ++ [ Css.justifyContent Css.spaceBetween ]


baseRowStyles : List Css.Style
baseRowStyles =
    [ Css.displayFlex
    , Css.width (Css.pct 100)
    , Css.alignItems Css.center
    ]


styleOf : Bool -> Dice -> List Css.Style
styleOf lockable dice =
    if lockable && dice.reboundsLeft == 0 then
        [ Css.cursor Css.pointer ]

    else
        []


baseColor : Float -> Css.Color
baseColor saturation =
    Css.hsl 204 1 saturation


palette =
    { ultraLight = baseColor 0.88
    , light = baseColor 0.7
    , medium = baseColor 0.5
    , white = Css.hex "#fff"
    }


baseShadow : Css.Style
baseShadow =
    Css.boxShadow4 (Css.px 0) (Css.px 2) (Css.px 6) (Css.hsla 0 0 0 0.2)


tableStyles : List Css.Style
tableStyles =
    [ Css.width (Css.pct 100)
    , Css.textAlign Css.center
    ]
