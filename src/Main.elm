module Main exposing (..)

import Html exposing (Html, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Html.App exposing (program)
import Matrix exposing (Matrix)
import Array


-- MODEL


type alias Model =
    { isOn : Matrix Bool }


init : ( Model, Cmd Msg )
init =
    ( { isOn = Matrix.repeat 5 5 True }, Cmd.none )



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
        ]


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


lightButton : Int -> Int -> Bool -> Html Msg
lightButton x y isOn =
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



-- UPDATE


type alias LightIndex =
    { x : Int, y : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle index ->
            ( { model | isOn = toggleLight index model.isOn }, Cmd.none )


toggleLight : LightIndex -> Matrix Bool -> Matrix Bool
toggleLight indexToToggle matrix =
    let
        toggleBool bool =
            not bool
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
