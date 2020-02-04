module Main exposing (..)

-- Render an interactive Qwixx scoresheet.
--
import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
--import Json.Encode as E exposing (Value, int, object, string, array, bool)

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }

-- MODEL
-- Can we just work with lists?
type alias Model =
  { redCheckedStates : Array Bool
  , redScore: Int
  , yellowCheckedStates : Array Bool
  , yellowScore: Int
  , greenCheckedStates : Array Bool
  , greenScore: Int
  , blueCheckedStates : Array Bool
  , blueScore: Int
  , penaltyStates: Array Bool
  , totalScore: Int
  }

init : () -> ( Model, Cmd msg )
init () =
  (
    { redCheckedStates  = Array.initialize 12 (always False)
    , redScore = 0
    , yellowCheckedStates  = Array.initialize 12 (always False)
    , yellowScore = 0
    , greenCheckedStates  = Array.initialize 12 (always False)
    , greenScore = 0
    , blueCheckedStates  = Array.initialize 12 (always False)
    , blueScore = 0
    , penaltyStates = Array.initialize 4 (always False)
    , totalScore = 0
    },
    Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch []

-- UPDATE

type Msg
  = UpdateCheckedState Int Bool String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateCheckedState idx isChecked color ->
      case color of
        "red" ->
          (
            { model | redCheckedStates = Array.set idx isChecked model.redCheckedStates }
            , Cmd.none
          )
        "yellow" ->
          (
            { model | yellowCheckedStates = Array.set idx isChecked model.yellowCheckedStates }
            , Cmd.none
          )
        "green" ->
          (
            { model | greenCheckedStates = Array.set idx isChecked model.greenCheckedStates }
            , Cmd.none
          )
        "blue" ->
          (
            { model | blueCheckedStates = Array.set idx isChecked model.blueCheckedStates }
            , Cmd.none
          )
        _ -> ( model, Cmd.none )

getValueElement: Int -> Html Msg
getValueElement idx =
  if idx == 11 then 
    img [src "lock.svg"] []
  else
    text (String.fromInt (idx + 2))

-- VIEW
rowBox: (Int, Bool, String) -> Html Msg
rowBox (idx, isChecked, color) =
  div 
    [ class "qwixx-box", onClick (UpdateCheckedState idx (not isChecked) color) ]
    [ div [ 
        classList [ ("checked", isChecked), ("not-checked", not isChecked) ]
      ]
      [ text "X" ]
    , div [ class "qwixx-value" ] [ getValueElement idx ]
    ]

rowMapper : (Int, Bool) -> String -> (Int, Bool, String)
rowMapper (idx, b) str =
  (idx, b, str)

row: Array Bool -> String -> Html Msg
row rowArray color =
    div [ class "qwixx-row", class color ]
      (List.map rowBox (List.map (\tup -> rowMapper tup color) (Array.toIndexedList rowArray)))

view : Model -> Html Msg
view model =
  div 
    [ class "qwixx-app" ]
    [ div
      [ class "qwixx-container"]
      [ row model.redCheckedStates "red"
      , row model.yellowCheckedStates "yellow"
      , row model.greenCheckedStates "green"
      , row model.blueCheckedStates "blue"
      , row model.penaltyStates "black"
      , row model.penaltyStates "black"
      ]
    ]
