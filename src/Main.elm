module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { plainText : String }


initialModel : Model
initialModel =
    { plainText = "Hey I'm a Plain Text" }


type Msg = Yeet

update : Msg -> Model -> Model
update msg model = model


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| model.plainText ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
