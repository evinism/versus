module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { plainText : String
    , mapping : List Char
    }


-- base helpers!!! --

mappingToString : List Char -> String
mappingToString mapping = List.map (String.fromChar) mapping |> (String.join "")

--obfuscateSingleCharacter : List Char -> Char -> Char
--obfuscateSingleCharacter mapping char = Char.toCode 
--    |> \code -> if ( || )
--    |>  Char.fromCode

--toObfuscatedText : String -> List Char -> String
--toObfuscatedText plainText mapping = split |> mapSingleChar mapping |> join

-- 


initialModel : Model
initialModel =
    { plainText = "Hey I'm a Plain Text"
    -- Implicitly associated by position to a, b, c, ...
    , mapping = ['i', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'a', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    }

type Msg = Yeet

update : Msg -> Model -> Model
update msg model = model


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| model.plainText ]
        , div [] [ text <| mappingToString model.mapping ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
