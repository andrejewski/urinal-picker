module Main exposing (Model, Msg(..), Occupant, OccupantHeight(..), Screen(..), Slot(..), background, emptyGenerator, foreground, getWithDefault, init, main, occupantGenerator, occupantTypeGenerator, pantColors, setupGame, shirtColors, skinColors, slotGenerator, subscriptions, timeInSeconds, update, view)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import List
import Random exposing (Generator)
import Task
import Time exposing (Posix)


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Screen
    = Welcome
    | Game
    | Finish
    | Info


type OccupantHeight
    = Short
    | Average
    | Tall


type alias Occupant =
    { height : OccupantHeight
    , isNude : Bool
    , shirtColor : String
    , pantColor : String
    , skinColor : String
    }


type Slot
    = Empty
    | Occupied Occupant


type alias Stage =
    List Slot


pantColors : Array String
pantColors =
    Array.fromList [ "darkred", "mediumvioletred", "orangered", "khaki", "darkkhaki", "rebeccapurple", "indigo", "slateblue", "darkslateblue", "darkgreen", "teal", "darkblue", "navy", "midnightblue", "tan", "maroon", "darkslategray" ]


shirtColors : Array String
shirtColors =
    Array.fromList [ "aliceblue", "antiquewhite", "aqua", "aquamarine", "azure", "beige", "bisque", "black", "blanchedalmond", "blue", "blueviolet", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornflowerblue", "cornsilk", "crimson", "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "darkgreen", "darkgrey", "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "deeppink", "deepskyblue", "dimgray", "dimgrey", "dodgerblue", "firebrick", "floralwhite", "forestgreen", "fuchsia", "gainsboro", "ghostwhite", "gold", "goldenrod", "gray", "green", "greenyellow", "grey", "honeydew", "hotpink", "indianred", "indigo", "ivory", "khaki", "lavender", "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan", "lightgoldenrodyellow", "lightgray", "lightgreen", "lightgrey", "lightpink", "lightsalmon", "lightseagreen", "lightskyblue", "lightslategray", "lightslategrey", "lightsteelblue", "lightyellow", "lime", "limegreen", "linen", "magenta", "maroon", "mediumaquamarine", "mediumblue", "mediumorchid", "mediumpurple", "mediumseagreen", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "mintcream", "mistyrose", "moccasin", "navajowhite", "navy", "oldlace", "olive", "olivedrab", "orange", "orangered", "orchid", "palegoldenrod", "palegreen", "paleturquoise", "palevioletred", "papayawhip", "peachpuff", "peru", "pink", "plum", "powderblue", "purple", "rebeccapurple", "red", "rosybrown", "royalblue", "saddlebrown", "salmon", "sandybrown", "seagreen", "seashell", "sienna", "silver", "skyblue", "slateblue", "slategray", "slategrey", "snow", "springgreen", "steelblue", "tan", "teal", "thistle", "tomato", "turquoise", "violet", "wheat", "whitesmoke", "yellow", "yellowgreen" ]


skinColors : Array String
skinColors =
    Array.fromList [ "#8d5524", "#c68642", "#e0ac69", "#f1c27d", "#ffdbac" ]


emptyGenerator : Generator Slot
emptyGenerator =
    Random.constant Empty


slotGenerator : Generator Slot
slotGenerator =
    Random.int 0 3
        |> Random.andThen
            (\i ->
                if i == 0 then
                    emptyGenerator

                else
                    occupantGenerator
                        |> Random.map Occupied
            )


getWithDefault : Int -> x -> Array.Array x -> x
getWithDefault index default array =
    case Array.get index array of
        Just x ->
            x

        Nothing ->
            default


occupantGenerator : Generator Occupant
occupantGenerator =
    Random.map5
        (\height isNude p sh sk ->
            { height = height
            , isNude = isNude
            , pantColor = getWithDefault p "black" pantColors
            , shirtColor = getWithDefault sh "black" shirtColors
            , skinColor = getWithDefault sk "#8d5524" skinColors
            }
        )
        occupantTypeGenerator
        (Random.weighted ( 19, False ) [ ( 1, True ) ])
        (Random.int
            0
            (Array.length pantColors)
        )
        (Random.int
            0
            (Array.length shirtColors)
        )
        (Random.int
            0
            (Array.length skinColors)
        )


occupantTypeGenerator : Generator OccupantHeight
occupantTypeGenerator =
    Random.weighted ( 2, Average ) [ ( 1, Short ), ( 1, Tall ) ]


stageGenerator : Int -> Generator Stage
stageGenerator count =
    let
        group =
            Random.pair
                (Random.list
                    count
                    slotGenerator
                )
                (Random.int
                    0
                    (count - 1)
                )
    in
    group
        |> Random.map
            (\( slots, emptyIndex ) ->
                slots
                    |> Array.fromList
                    |> Array.set emptyIndex Empty
                    |> Array.toList
            )


type alias Model =
    { screen : Screen
    , urinalCount : Int
    , slots : Stage
    , start : Posix
    , finish : Posix
    , stageCount : Int
    }


type Msg
    = Begin
    | Retry
    | ReturnHome
    | ShowInfo
    | Win
    | Lose
    | StartStage Stage
    | StartGame Posix
    | FinishGame Posix


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { screen = Welcome
      , urinalCount = 7
      , slots = []
      , start = Time.millisToPosix 0
      , finish = Time.millisToPosix 0
      , stageCount = 0
      }
    , Cmd.none
    )


setupGame : Model -> ( Model, Cmd Msg )
setupGame model =
    ( { model | screen = Game }, Task.perform StartGame Time.now )


prepStage : Model -> ( Model, Cmd Msg )
prepStage model =
    ( model
    , model.urinalCount
        |> stageGenerator
        |> Random.generate StartStage
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Begin ->
            setupGame model

        Retry ->
            setupGame model

        ReturnHome ->
            ( { model | screen = Welcome }, Cmd.none )

        ShowInfo ->
            ( { model | screen = Info }, Cmd.none )

        Win ->
            prepStage { model | stageCount = model.stageCount + 1 }

        Lose ->
            ( { model | screen = Finish }, Task.perform FinishGame Time.now )

        StartStage stage ->
            ( { model | slots = stage }, Cmd.none )

        StartGame start ->
            prepStage { model | start = start, stageCount = 0 }

        FinishGame finish ->
            ( { model | finish = finish }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ background model, foreground model ]


timeInSeconds : Posix -> Posix -> String
timeInSeconds start finish =
    let
        startMs =
            Time.posixToMillis start

        finishMs =
            Time.posixToMillis finish

        duration =
            finishMs - startMs

        seconds =
            duration // 1000
    in
    String.fromInt seconds


foreground : Model -> Html Msg
foreground model =
    case model.screen of
        Welcome ->
            div [ class "overlay" ]
                [ div
                    [ class "overlay-content home" ]
                    [ h1 [ class "title" ]
                        [ text "Urinal Picker" ]
                    , p [ class "button-set" ]
                        [ button [ class "button button-primary", onClick Begin ] [ text "Start Picking" ]
                        , button [ class "button button-secondary", onClick ShowInfo ] [ text "Huh?" ]
                        ]
                    ]
                ]

        Finish ->
            let
                seconds =
                    timeInSeconds model.start model.finish

                secondsUnit =
                    if seconds == "1" then
                        "second"

                    else
                        "seconds"

                urinals =
                    String.fromInt model.stageCount

                urinalsUnit =
                    if urinals == "1" then
                        "urinal"

                    else
                        "urinals"
            in
            div [ class "overlay" ]
                [ div
                    [ class "overlay-content home" ]
                    [ h1 [ class "score" ]
                        [ text ("You correctly picked " ++ urinals ++ " " ++ urinalsUnit)
                        , br [] []
                        , text ("in " ++ seconds ++ " " ++ secondsUnit ++ ".")
                        ]
                    , p [ class "button-set" ]
                        [ button [ class "button button-primary", onClick Retry ] [ text "Try again" ]
                        , button [ class "button button-secondary", onClick ReturnHome ] [ text "Return home" ]
                        ]
                    ]
                ]

        Game ->
            stageView model

        Info ->
            infoView


isEmpty : Slot -> Bool
isEmpty slot =
    case slot of
        Empty ->
            True

        Occupied _ ->
            False


isNudist : Slot -> Bool
isNudist slot =
    case slot of
        Empty ->
            False

        Occupied occupant ->
            occupant.isNude


maybeMax : Maybe Int -> Maybe Int -> Maybe Int
maybeMax x y =
    case ( x, y ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just a, Just b ) ->
            max a b |> Just


smallest : Maybe Int -> Maybe Int -> Maybe Int
smallest a b =
    case ( a, b ) of
        ( Just x, Just y ) ->
            min x y |> Just

        ( Just x, _ ) ->
            Just x

        ( _, Just y ) ->
            Just y

        _ ->
            Nothing


distanceFromNudist : Array Slot -> Int -> Maybe Int
distanceFromNudist allSlots index =
    smallest
        (distanceFromLeftNudist allSlots index)
        (distanceFromRightNudist allSlots index)


distanceFromLeftNudist : Array Slot -> Int -> Maybe Int
distanceFromLeftNudist allSlots index =
    Array.get index allSlots
        |> Maybe.andThen
            (\slot ->
                if isNudist slot then
                    Just 0

                else
                    distanceFromLeftNudist allSlots (index - 1)
                        |> Maybe.map (\x -> x + 1)
            )


distanceFromRightNudist : Array Slot -> Int -> Maybe Int
distanceFromRightNudist allSlots index =
    Array.get index allSlots
        |> Maybe.andThen
            (\slot ->
                if isNudist slot then
                    Just 0

                else
                    distanceFromRightNudist allSlots (index + 1)
                        |> Maybe.map (\x -> x + 1)
            )


isFarthestFromNudists : Array Slot -> Int -> Bool
isFarthestFromNudists allSlots chosenSlot =
    let
        maximumDistance =
            allSlots
                |> Array.indexedMap
                    (\i slot ->
                        if isEmpty slot then
                            distanceFromNudist allSlots i

                        else
                            Just 0
                    )
                |> Array.foldl
                    maybeMax
                    (Just 0)

        chosenDistance =
            distanceFromNudist allSlots chosenSlot
    in
    chosenDistance == maximumDistance


neighborHeight : Maybe Slot -> Int
neighborHeight maybeSlot =
    case maybeSlot of
        Just slot ->
            case slot of
                Occupied occupant ->
                    case occupant.height of
                        Tall ->
                            3

                        Average ->
                            2

                        Short ->
                            1

                Empty ->
                    0

        Nothing ->
            0


adjacentHeight : Array Slot -> Int -> Int
adjacentHeight allSlots chosenSlot =
    let
        left =
            Array.get (chosenSlot - 1) allSlots
                |> neighborHeight

        right =
            Array.get (chosenSlot + 1) allSlots
                |> neighborHeight
    in
    left + right


isAdjacentTheShortestOccupants : Array Slot -> Int -> Bool
isAdjacentTheShortestOccupants allSlots chosenSlot =
    let
        maxHeight =
            8

        minimumHeight =
            allSlots
                |> Array.indexedMap
                    (\i slot ->
                        if isEmpty slot then
                            adjacentHeight allSlots i

                        else
                            maxHeight
                    )
                |> Array.foldl min maxHeight

        chosenHeight =
            adjacentHeight allSlots chosenSlot
    in
    chosenHeight == minimumHeight


isOptimalSlot : Array Slot -> Int -> Bool
isOptimalSlot allSlots chosenIndex =
    case Array.get chosenIndex allSlots of
        Just slot ->
            if slot |> isEmpty |> not then
                False

            else if allSlots |> Array.toList |> List.any isNudist then
                isFarthestFromNudists allSlots chosenIndex

            else
                isAdjacentTheShortestOccupants allSlots chosenIndex

        Nothing ->
            False


stageView : Model -> Html Msg
stageView model =
    let
        slotArray =
            model.slots
                |> Array.fromList

        slots =
            slotArray
                |> Array.indexedMap
                    (\index slot ->
                        let
                            action =
                                if isOptimalSlot slotArray index then
                                    Win

                                else
                                    Lose

                            entry =
                                case slot of
                                    Empty ->
                                        div [ class "slot-empty" ] []

                                    Occupied occupant ->
                                        div [ class "slot-occupied" ] [ occupantView occupant ]
                        in
                        div
                            [ class "slot"
                            , onClick action
                            ]
                            [ entry ]
                    )
    in
    div []
        [ div [ class "slots overlay" ]
            (slots
                |> Array.toList
            )
        ]


bg : String -> Html.Attribute Msg
bg color =
    style "background-color" color


heightToString : OccupantHeight -> String
heightToString kind =
    case kind of
        Tall ->
            "tall"

        Average ->
            "avaerage"

        Short ->
            "short"


occupantView : Occupant -> Html Msg
occupantView occupant =
    let
        shirtColor =
            if occupant.isNude then
                occupant.skinColor

            else
                occupant.shirtColor

        pantColor =
            if occupant.isNude then
                occupant.skinColor

            else
                occupant.pantColor

        kindClass =
            occupant.height |> heightToString |> (++) "occupant--"
    in
    div [ class "occupant", class kindClass ]
        [ div [ class "occupant-head", bg occupant.skinColor ]
            [ div [ class "occupant-neck", bg occupant.skinColor ] [] ]
        , div
            [ class "occupant-shirt", bg shirtColor ]
            []
        , div [ class "occupant-waist", bg pantColor ] []
        , div
            [ class "occupant-legs" ]
            [ div [ class "occupant-leg", bg pantColor ]
                []
            , div
                [ class "occupant-leg", bg pantColor ]
                []
            ]
        ]


background : Model -> Html Msg
background model =
    let
        urinals =
            List.repeat model.urinalCount 0
                |> List.map
                    (\x ->
                        div
                            [ class "urinal"
                            ]
                            [ div [ class "urinal-walls" ]
                                [ div [ class "urinal-hole" ] []
                                ]
                            ]
                    )
    in
    div [ class "background" ]
        [ div [ class "bathroom-scene" ]
            [ div [ class "bathroom-wall" ] []
            , div [ class "urinal-row" ] urinals
            , div [ class "bathroom-floor" ]
                (if model.screen == Game then
                    [ text "Urinal Picker" ]

                 else
                    []
                )
            ]
        ]


infoView : Html Msg
infoView =
    div [ class "info overlay" ]
        [ header [ onClick ReturnHome ] [ text "← Back to home screen" ]
        , article []
            [ h1 [] [ text "Urinal Picker" ]
            , h2 [] [ text "One of the hardest recurring tasks in a man's life is deciding which urinal to use. It takes snap judgement and application of an unspoken rule set. That critical thinking is what Urinal Picker captures. If you are a man, learn to practice our art. Otherwise, share in our struggle through this challenge." ]
            , p []
                [ text "Urinal Picker is written by "
                , a [ href "https://jew.ski/" ] [ text "Chris Andrejewski" ]
                , text ". The source code is "
                , a [ href "https://github.com/andrejewski/urinal-picker" ] [ text "open source" ]
                , text "."
                ]
            ]
        ]
