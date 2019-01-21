import Browser
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Regex
--import Debug

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { domain: String
  , formNum: Int
  , index: Int
  , codomain: String
  , pattern: String
  , concatenated: String
  }

init : Model
init =
  { domain = ""
  , formNum = 1
  , index = 0
  , codomain = ""
  , pattern = ""
  , concatenated = "Concatenated string comes here."
  }


-- UPDATE

type Msg = OnDomain String | OnRegex String | OnIndex String

update : Msg -> Model -> Model
update msg model =
  case msg of
    OnDomain t -> let match = if String.length model.pattern > 0 then
                                case getAt model.index <| applyRegex model.pattern t of
                                  Just s -> s
                                  Nothing -> ""
                              else t
                  in { model | domain = t, codomain = match }
    OnRegex  r -> let match = if String.length r > 0 then
                                case getAt model.index <| applyRegex r model.domain of
                                  Just s -> s
                                  Nothing -> ""
                              else model.domain
                  in { model | pattern = r, codomain = match }
    OnIndex  i -> let index = case String.toInt i of
                                Just i_ -> i_
                                Nothing -> 0
                      match = if String.length model.pattern > 0 then
                                case getAt index <| applyRegex model.pattern model.domain of
                                  Just s -> s
                                  Nothing -> ""
                              else model.domain
                  in { model | index = index, codomain = match }

getAt : Int -> List a -> Maybe a
getAt i xs = List.head <| List.drop i xs

-- WARNING: It's NOT tail-recursive!
replace : Int -> a -> List a -> List a
replace target y xs = case xs of
  [] -> []
  (h::t) ->
    if target < 1 then xs
    else if target > List.length xs then xs
    else if target == 1 then y :: t
    else h :: (replace (target - 1) y t)

applyRegex : String -> String -> List String
applyRegex pattern domain =
  let matches = Regex.find (by pattern) domain
  in  List.map (\m -> m.match) matches
  
by : String -> Regex.Regex
by pattern = Maybe.withDefault Regex.never <| Regex.fromString pattern

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div []
          [ h1 [] [text "Regex & Concatenation"]]
    , hr [] []
    , div []
          [ h2 [] [ text "Original Text" ]
          , textarea [placeholder "before", cols 100, rows 20, onInput OnDomain ] [ text model.domain ]
          ]
    , hr [] []
    , div []
          [ h2 [] [text "Concatenated Text" ]
          , p [] [text model.concatenated ]
          ]
    , hr [] []
    , div []
          [ div []
                [ button [] [ text "add before" ]
                ]
          , h2 [] [ text <| "Regex Pattern " ++ String.fromInt 1 ]
          , div []
                [ input [ placeholder "regex", onInput OnRegex ] [] ]
          , div []
                [ input [ placeholder "index (default: 0)", onInput OnIndex ] [] ]
          ]
          , div []
                [ textarea [ placeholder "after", cols 100, rows 20, readonly True ] [ text model.codomain ] ]
          , div []
                [ button [] [ text "add after" ]
                ]
    ]

