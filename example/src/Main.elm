module Main exposing (..)


import Browser
import Html exposing (Html, text)
import Html.Events exposing (onClick)
import Adapter.Pom

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = ClickedButton


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedButton ->
            model + 1


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text <| String.fromInt model
        , Html.button [ onClick ClickedButton ] [ Html.text "Click me!" ]
        ]
