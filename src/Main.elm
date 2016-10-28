module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Html.App exposing (program)
import Matrix exposing (Matrix)
import Array


-- MODEL


type alias Model =
    { isOn : Matrix (Maybe Bool) }


init : ( Model, Cmd Msg )
init =
    ( { isOn =
            Matrix.repeat 6 6 (Just True)
                |> Matrix.set 4 3 Nothing
                |> Matrix.set 2 2 Nothing
                |> Matrix.set 1 1 (Just False)
                |> Matrix.set 2 4 (Just False)
                |> Matrix.set 3 1 (Just False)
                |> Matrix.set 2 5 (Just False)
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Toggle LightIndex



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ model.isOn
            |> Matrix.indexedMap lightButton
            |> matrixToDivs
        , div
            []
            [ solvedMsg (isSolved model)
            ]
        ]


solvedMsg : Bool -> Html.Html Msg
solvedMsg bool =
    if bool then
        text "Well Done! You solved the puzzle!"
    else
        text "Keep Trying!"


isSolved : Model -> Bool
isSolved model =
    let
        isOn maybeIsOn =
            maybeIsOn
                |> Maybe.withDefault False

        onLights =
            Matrix.filter isOn model.isOn
    in
        Array.isEmpty onLights


matrixToDivs : Matrix (Html.Html Msg) -> Html.Html Msg
matrixToDivs matrix =
    let
        makeRow y =
            Matrix.getRow y matrix
                |> Maybe.map (Array.toList)
                |> Maybe.withDefault []
                |> Html.div []

        height =
            Matrix.height matrix
    in
        [0..height]
            |> List.map makeRow
            |> Html.div []


lightButton : Int -> Int -> Maybe Bool -> Html Msg
lightButton x y maybeIsOn =
    case maybeIsOn of
        Just isOn ->
            div
                [ style
                    [ ( "width", "35px" )
                    , ( "height", "35px" )
                    , ( "margin", "2px" )
                    , ( "display", "inline-block" )
                    , ( "background-color"
                      , if isOn then
                            "red"
                        else
                            "grey"
                      )
                    ]
                , onClick (Toggle { x = x, y = y })
                ]
                []

        Nothing ->
            div
                [ style
                    [ ( "width", "35px" )
                    , ( "height", "35px" )
                    , ( "margin", "2px" )
                    , ( "display", "inline-block" )
                    ]
                ]
                []



-- UPDATE


type alias LightIndex =
    { x : Int, y : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle index ->
            ( { model | isOn = toggleLight index model.isOn }, Cmd.none )


toggleLight : LightIndex -> Matrix (Maybe Bool) -> Matrix (Maybe Bool)
toggleLight indexToToggle matrix =
    let
        toggleBool bool =
            Maybe.map not bool
    in
        matrix
            |> Matrix.update indexToToggle.x indexToToggle.y toggleBool
            |> Matrix.update (indexToToggle.x + 1) indexToToggle.y toggleBool
            |> Matrix.update (indexToToggle.x - 1) indexToToggle.y toggleBool
            |> Matrix.update indexToToggle.x (indexToToggle.y + 1) toggleBool
            |> Matrix.update indexToToggle.x (indexToToggle.y - 1) toggleBool



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
