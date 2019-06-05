module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra


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

type Msg = Swap Char Char


--- helpers for update!!!

-- stupid stuff
findCharPos : Char -> List Char -> Int
findCharPos toFind mapping = (List.Extra.elemIndex toFind mapping)
    |> Maybe.withDefault 0 -- We assume this is never hit


swapChars : Char -> Char -> List Char -> List Char
swapChars firstChar secondChar mapping = 
    let 
        firstCharPos = findCharPos firstChar mapping
        secondCharPos = findCharPos secondChar mapping
    in List.Extra.swapAt firstCharPos secondCharPos mapping

--- 

update : Msg -> Model -> Model
update msg model =
    case msg of
        Swap char1 char2 ->
            { model | mapping = swapChars char1 char2 model.mapping }

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| model.plainText ]
        , div [] [ text <| mappingToString model.mapping ]
        , button [ onClick (Swap 'a' 'i') ] [ text "yeet" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
