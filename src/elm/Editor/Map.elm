module Editor.Map exposing (Model, Msg(..), init, update, view)

import Helper exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Maybe


type alias Model =
    { regexAll : String
    , indexAll : Int
    , sections : List Section
    }


type alias Section =
    { domain : String
    , codomain : String
    }


init : Model
init =
    { regexAll = ""
    , indexAll = 0
    , sections = []
    }


type Msg
    = RegexAll String
    | IndexAll Int
    | Open (List String)


update : Msg -> Model -> Model
update msg model =
    let
        currentSep =
            List.map (\s -> s.domain) model.sections
    in
    case msg of
        RegexAll regexAll ->
            let
                newSections =
                    List.map
                        (\sep ->
                            { domain = sep
                            , codomain = Maybe.withDefault "" <| getAt model.indexAll <| List.map .match <| applyRegex regexAll sep
                            }
                        )
                        currentSep
            in
            { model
                | regexAll = regexAll
                , sections = newSections
            }

        IndexAll indexAll ->
            let
                newSections =
                    List.map
                        (\sep ->
                            { domain = sep
                            , codomain = Maybe.withDefault "" <| getAt indexAll <| List.map .match <| applyRegex model.regexAll sep
                            }
                        )
                        currentSep
            in
            { model
                | indexAll = indexAll
                , sections = newSections
            }

        Open separated ->
            let
                newSections =
                    List.map
                        (\sep ->
                            { domain = sep
                            , codomain = Maybe.withDefault "" <| getAt model.indexAll <| List.map .match <| applyRegex model.regexAll sep
                            }
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
            , input [ onInput RegexAll, style "width" "80%", value model.regexAll ] []
            ]
        , hr [] []
        , div []
            [ h2 [] [ text "Index" ]
            , input [ onInput (toInt >> IndexAll), value <| String.fromInt model.indexAll ] []
            ]
        , hr [] []
        , div [] <|
            makeSections model.sections
        ]


makeSections : List Section -> List (Html Msg)
makeSections ss =
    List.map
        (\s ->
            div []
                [ textarea [ style "width" "40%", rows 10, readonly True ] [ text s.domain ]
                , textarea [ style "width" "40%", rows 10, readonly True, placeholder "no match" ] [ text s.codomain ]
                ]
        )
        ss
