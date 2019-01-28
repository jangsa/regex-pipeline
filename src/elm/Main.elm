module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as JD
import Regex

import Editor.Separate as ESE
import Editor.Map as EM
import Editor.Singleton as ESI

import Debug

main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { currentEditor : Editor
    , separateModel : ESE.Model
    , mapModel : EM.Model
    , singletonModel : ESI.Model
    }


init : Model
init =
    { currentEditor = SeparateEditor
    , separateModel = ESE.init
    , mapModel = EM.init
    , singletonModel = ESI.init
    }


type Msg
    = Separate ESE.Msg
    | Map EM.Msg
    | Singleton ESI.Msg

type Editor
    = SeparateEditor
    | MapEditor
    | SingletonEditor

update : Msg -> Model -> Model
update msg model =
    case msg of
        Separate subMsg ->
            { model | currentEditor = SeparateEditor, separateModel = ESE.update subMsg model.separateModel }

        Map subMsg ->
            { model | currentEditor = MapEditor, mapModel = EM.update subMsg model.mapModel }

        Singleton subMsg ->
            { model | currentEditor = SingletonEditor, singletonModel = ESI.update subMsg model.singletonModel }


view : Model -> Html Msg
view model =
    let
        editor =
            case model.currentEditor of
                SeparateEditor -> Html.map Separate <| ESE.view model.separateModel
                MapEditor -> Html.map Map <| EM.view model.mapModel
                SingletonEditor -> Html.map Singleton <| ESI.view model.singletonModel
    in
        div []
            [ ul []
                 [ li [ onClick <| Separate ESE.Open ] [ text "separate" ]
                 , li [ onClick <| Map <| EM.Open model.separateModel.separated ] [ text "map" ]
                 , li [ onClick <| Singleton ESI.Open ] [ text "singleton" ]
                 ]
            , editor
            ]
