module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Process
import Task
import List.Extra exposing (takeWhile)


animations : List Animation
animations =
    (List.take 5 steps |> List.map Walk)
        ++ (dialogs |> List.map Dialog)
        ++ (List.drop 5 steps |> List.map Walk)


dialogs : List String
dialogs =
    [ "Hey, why are you sleeping here?"
    , "Are you okay?"
    , "Should I call for help?"
    , "Oh, you woke up!"
    , "You had me worried for a bit."
    ]


steps : List Step
steps =
    [ { x = 0, y = 0, dir = South }
    , { x = 1, y = 0, dir = East }
    , { x = 1, y = 1, dir = South }
    , { x = 1, y = 2, dir = South }
    , { x = 1, y = 3, dir = South }
    , { x = 2, y = 3, dir = East }
    , { x = 2, y = 2, dir = North }
    , { x = 3, y = 2, dir = East }
    , { x = 3, y = 1, dir = North }
    , { x = 3, y = 0, dir = North }
    , { x = 2, y = 0, dir = West }
    , { x = 1, y = 0, dir = West }
    , { x = 0, y = 0, dir = West }
    ]


firstStep : Step
firstStep =
    steps
        |> List.head
        |> Maybe.withDefault (Step 0 0 South)


lastAnimation : Animation
lastAnimation =
    animations
        |> List.reverse
        |> List.head
        |> Maybe.withDefault (Walk { x = 0, y = 0, dir = South })


type alias Model =
    { step : Int
    , showMove : Bool
    }


type Direction
    = North
    | South
    | West
    | East


type Animation
    = Walk Step
    | Dialog String


type alias Step =
    { x : Int, y : Int, dir : Direction }


type Msg
    = Play
    | NextStep


init : ( Model, Cmd Msg )
init =
    ( Model 0 True, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ]
        , button [ onClick Play ] [ text "Play" ]
        , button [ onClick NextStep ] [ text "Next" ]
        , div [ style [ ( "position", "relative" ) ] ]
            [ viewBlock model ]
        ]


getAnimation : Int -> Animation
getAnimation index =
    animations
        |> List.drop index
        |> List.head
        |> Maybe.withDefault lastAnimation


viewBlock : Model -> Html msg
viewBlock model =
    let
        animation : Animation
        animation =
            getAnimation model.step

        position : Step
        position =
            case animation of
                Walk step ->
                    step

                Dialog _ ->
                    animations
                        |> List.indexedMap (,)
                        |> takeWhile
                            (\( i, animation ) ->
                                case animation of
                                    Walk _ ->
                                        i <= model.step

                                    Dialog _ ->
                                        False
                            )
                        |> List.map Tuple.second
                        |> List.map
                            (\animation ->
                                case animation of
                                    Walk step ->
                                        step

                                    Dialog _ ->
                                        Step 0 0 South
                            )
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault firstStep

        dialog : String
        dialog =
            case animation of
                Walk _ ->
                    ""

                Dialog string ->
                    string
    in
        div []
            [ div [ style (styleBlock model.showMove position) ]
                [ div [ style (styleBlockDir position.dir) ] [] ]
            , div [] [ text dialog ]
            ]


styleBlock : Bool -> Step -> List ( String, String )
styleBlock showMove { x, y } =
    [ ( "width", "50px" )
    , ( "height", "50px" )
    , ( "position", "absolute" )
    , ( "left", toString (50 * x) ++ "px" )
    , ( "top", toString (50 * y) ++ "px" )
    , ( "background-color", "black" )
    , ( "transition"
      , if showMove then
            "left "
                ++ toString sleep
                ++ "ms linear, top "
                ++ toString sleep
                ++ "ms linear"
        else
            ""
      )
    ]


styleBlockDir : Direction -> List ( String, String )
styleBlockDir dir =
    let
        styleDir =
            case dir of
                North ->
                    [ ( "left", "40%" ), ( "top", "10%" ) ]

                South ->
                    [ ( "left", "40%" ), ( "top", "70%" ) ]

                West ->
                    [ ( "left", "10%" ), ( "top", "40%" ) ]

                East ->
                    [ ( "left", "70%" ), ( "top", "40%" ) ]

        styles =
            [ ( "background-color", "red" )
            , ( "position", "absolute" )
            , ( "width", "20%" )
            , ( "height", "20%" )
            ]
    in
        styles ++ styleDir


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            ( { model | step = 0, showMove = False }, nextStep )

        NextStep ->
            let
                step : Int
                step =
                    model.step + 1
            in
                if step > List.length animations then
                    ( model, Cmd.none )
                else
                    case getAnimation step of
                        Walk _ ->
                            ( { model | step = step, showMove = True }
                            , nextStep
                            )

                        Dialog _ ->
                            ( { model | step = step, showMove = True }
                            , Cmd.none
                            )


sleep : Float
sleep =
    500


nextStep : Cmd Msg
nextStep =
    delay sleep NextStep


delay : Float -> msg -> Cmd msg
delay ms msg =
    Process.sleep ms
        |> Task.andThen (\_ -> Task.succeed msg)
        |> Task.perform identity


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
