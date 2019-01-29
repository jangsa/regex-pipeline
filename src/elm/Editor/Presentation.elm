module Editor.Presentation exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)

import Helper exposing (..)


type alias Model =
    { prefix : String
    , postfix : String
    , matched : List String
    , concatenated : List String
    }


init : Model
init =
    { prefix = ""
    , postfix = ""
    , matched = []
    , concatenated = []
    }


type Msg
    = Prefix String
    | Postfix String
    | Open (List String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Prefix prefix ->
            let
                withPostfix =
                    List.map (\m -> m ++ model.postfix) model.matched
            in
            { model | prefix = prefix, concatenated = List.map (\s -> prefix ++ s) withPostfix }

        Postfix postfix ->
            let
                withPrefix =
                    List.map (\m -> model.prefix ++ m) model.matched
            in
            { model | postfix = postfix, concatenated = List.map (\s -> s ++ postfix) withPrefix }

        Open matched ->
            let
                matchedNotEmpty =
                    List.filter (String.isEmpty >> not) matched
            in
            { model | matched = matchedNotEmpty, concatenated = List.map (\s -> model.prefix ++ s ++ model.postfix) matchedNotEmpty }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Presentation page" ]
        , hr [] []
        , div []
            [ h2 [] [ text "Prefix" ]
            , input [ onInput Prefix, style "width" "80%", value model.prefix ] []
            ]
        , hr [] []
        , div []
            [ h2 [] [ text "Postfix" ]
            , input [ onInput Postfix, style "width" "80%", value <| model.postfix ] []
            ]
        , hr [] []
        , div [] <|
            List.map2
                (\m ->
                    \c ->
                        div []
                            [ textarea [ style "width" "40%", rows 10, readonly True, placeholder "no match" ] [ text c ]
                            ]
                )
                model.matched
                model.concatenated
        ]
