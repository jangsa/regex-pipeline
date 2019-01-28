module Editor.Separate exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as JD
import Regex
import Maybe

type alias Model =
    { text : String
    , regex : String
    , separated : List String
    }


init : Model
init =
    { text = ""
    , regex = ""
    , separated = []
    }


type Msg
    = Text String
    | Regex String
    | Open


update : Msg -> Model -> Model
update msg model =
    let
        makeRule r =
            Maybe.withDefault Regex.never <|
                Regex.fromString r
        separate r t =
            if String.length r > 0 then
                Regex.split (makeRule r) t
            else []
    in
        case msg of
            Text text ->
                { model | text = text, separated = separate model.regex text }
    
            Regex regex ->
                { model | regex = regex, separated = separate regex model.text }
    
            Open ->
                model


view model =
    div []
        [ div []
              [ h1 [] [ text "Separate Editor" ]
              ]
        , hr [] []
        , div []
              [ h2 [] [ text "Original Text" ]
              , textarea [ onInput Text, style "width" "80%", rows 30 ] [ text model.text]
              ]
        , hr [] []
        , div []
              [ h2 [] [ text "Regex" ]
              , input [ onInput Regex, style "width" "80%", value model.regex ] []
              ]
        , hr [] []
        , div []
              [ h2 [] [ text "Separated" ]
              , div [] <|
                  List.map
                  (\sep -> p [] [ text sep ])
                  model.separated
              ]
        ]
