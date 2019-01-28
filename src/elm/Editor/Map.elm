module Editor.Map exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as JD
import Regex
import Debug

type alias Model =
    { regexAll : String
    , indexAll : String
    , sections : List Section
    }

type alias Section =
    { domain : String
    , codomain : String
    }

init : Model
init =
    { regexAll = ""
    , indexAll = ""
    , sections = []
    }


type Msg
    = RegexAll String
    | IndexAll String
    | Open (List String)


-- TODO: Apply regex to each codomain
update : Msg -> Model -> Model
update msg model =
    case msg of
        RegexAll regexAll ->
            { model | regexAll = regexAll }

        IndexAll indexAll ->
            { model | indexAll = indexAll }

        Open separated ->
            let
                newSections =
                    List.map
                    (\sep ->
                        { domain = sep, codomain = "" }
                    )
                    separated
            in
                { model | sections = newSections }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Map page" ]
        , hr [] []
        , div []
              [ h2 [] [ text "Regex" ]
              , input [ style "width" "80%" ] [ text model.regexAll ]
              ]
        , hr [] []
        , div []
              [ h2 [] [ text "Index" ]
              , input [ value model.indexAll ] []
              ]
        , hr [] []
        , div [] <|
              sections model.sections
        ]

sections : List Section -> List (Html Msg)
sections ss =
    List.map
    (\s ->
        div []
            [ textarea [ style "width" "40%", readonly True ] [ text s.domain ]
            , textarea [ style "width" "40%", readonly True ] [ text s.codomain ]
            ]
    )
    ss

