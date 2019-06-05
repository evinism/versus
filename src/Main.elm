module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra
import Set exposing (Set)


type alias Model =
    { plainText : String
    , mapping : List Char
    , heldKeys : Set String
    }



-- base helpers!!! --


mappingToString : List Char -> String
mappingToString mapping =
    List.map String.fromChar mapping |> String.join ""


obfuscateSingleCharacter : List Char -> Char -> Char
obfuscateSingleCharacter mapping char =
    let
        index =
            Char.toCode char
                - 97
    in
    case char of
        ' ' ->
            ' '

        _ ->
            List.drop index mapping
                |> List.head
                -- TODO: this is v dangerous
                |> Maybe.withDefault char


toObfuscatedText : String -> List Char -> String
toObfuscatedText plainText mapping =
    String.toLower plainText
        -- filter out anything but spaces and letters
        |> String.filter (\char -> Char.isAlpha char || char == ' ')
        |> String.toList
        |> List.map (obfuscateSingleCharacter mapping)
        |> List.map String.fromChar
        |> String.join ""



--


initialModel : Model
initialModel =
    { plainText = "Hey I'm a Plain Text"

    -- Implicitly associated by position to a, b, c, ...
    , mapping = [ 'i', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'a', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]
    , heldKeys = Set.empty
    }


type Msg
    = Swap Char Char
    | Press String
    | Release String



--- helpers for update!!!
-- stupid stuff


findCharPos : Char -> List Char -> Int
findCharPos toFind mapping =
    List.Extra.elemIndex toFind mapping
        -- We assume this is never hit
        |> Maybe.withDefault 0


swapChars : Char -> Char -> List Char -> List Char
swapChars firstChar secondChar mapping =
    let
        firstCharPos =
            findCharPos firstChar mapping

        secondCharPos =
            findCharPos secondChar mapping
    in
    List.Extra.swapAt firstCharPos secondCharPos mapping



---


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Swap char1 char2 ->
            ( { model | mapping = swapChars char1 char2 model.mapping }, Cmd.none )

        Press str ->
            ( { model | heldKeys = Set.insert str model.heldKeys }, Cmd.none )

        Release str ->
            ( { model | heldKeys = Set.remove str model.heldKeys }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        foo = Debug.log "held keys" model.heldKeys
    in
    
    div []
        [ div [] [ text <| model.plainText ]
        , div [] [ text <| mappingToString model.mapping ]
        , button [ onClick (Swap 'a' 'i') ] [ text "yeet" ]
        , div [] [ text <| toObfuscatedText model.plainText model.mapping ]
        ]


toPress : String -> Msg
toPress =
    Press


toRelease : String -> Msg
toRelease =
    Release


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyPress (Decode.map toPress (Decode.field "key" Decode.string))
        , Browser.Events.onKeyUp (Decode.map toRelease (Decode.field "key" Decode.string))
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
