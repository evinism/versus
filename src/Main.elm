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


-- this is garbo
isAllowedString : String -> Bool
isAllowedString string = (String.length string) == 1 && 
    case (String.uncons string) of
        Just (char, _) -> isAllowedChar char
        Nothing -> False

isAllowedChar : Char -> Bool
isAllowedChar char = Char.isAlpha char || char == ' '

toObfuscatedText : String -> List Char -> String
toObfuscatedText plainText mapping =
    String.toLower plainText
        -- filter out anything but spaces and letters
        |> String.filter isAllowedChar
        |> String.toList
        |> List.map (obfuscateSingleCharacter mapping)
        |> List.map String.fromChar
        |> String.join ""



--


initialModel : Model
initialModel =
    { plainText = "It seems probable to me, that God in the beginning formed matter in solid, massy, hard, impenetrable, moveable particles, of such sizes and figures, and with such other properties, and in such proportions to space, as most conducted to the ends for which He formed them; and that these primitive particles being solids, are incomparably harder than any porous bodies compounded of them, even so very hard, as never to wear or break in pieces; no ordinary power being able to divide what God Himself made one in the first creation."

    -- Implicitly associated by position to a, b, c, ...
    , mapping = ['n', 'c', 'm', 'h', 'x', 'g', 'e', 'y', 'w', 'u', 'f', 'd', 'z', 'q', 'v', 'a', 'k', 'l', 'i', 't', 'b', 'p', 'j', 's', 'r', 'o']
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


addToHeld : String -> Model -> Model
addToHeld lowerStr model = 
    let newHeldKeys = Set.insert lowerStr model.heldKeys
        mapping =
            case Set.toList newHeldKeys of
                a :: b :: [] ->
                    case (String.uncons a, String.uncons b) of
                        (Just (aa, _), Just (bb, _)) ->
                            swapChars aa bb model.mapping
                        _ -> model.mapping
                _ ->
                    model.mapping
    in
    { model 
    | heldKeys = newHeldKeys
    , mapping = mapping }

removeFromHeld : String -> Model -> Model
removeFromHeld lowerStr model = { model | heldKeys = Set.remove lowerStr model.heldKeys}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Swap char1 char2 ->
            ( { model | mapping = swapChars char1 char2 model.mapping }, Cmd.none )

        Press rawString ->
            let lowerStr = String.toLower rawString
            in
            if isAllowedString lowerStr then
                (addToHeld lowerStr model, Cmd.none)
                else (model, Cmd.none)

        Release rawString ->
            let lowerStr = String.toLower rawString
            in (removeFromHeld lowerStr model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        foo = Debug.log "held keys" model.heldKeys
    in
    
    div []
        [ button [ onClick (Swap 'a' 'i') ] [ text "yeet" ]
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
