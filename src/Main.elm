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
init flags =
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

-- encodeModel : Model -> Value
-- encodeModel model =
--   E.object
--     [ ( "red", (E.list E.bool (Array.toList model.redCheckedStates)))
--     , ( "yellow", (E.list E.bool (Array.toList model.yellowCheckedStates)))
--     , ( "green", (E.list E.bool (Array.toList model.greenCheckedStates)))
--     , ( "blue", (E.list E.bool (Array.toList model.blueCheckedStates)))
--     ]

-- port jsonConsole : Value -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch []

-- UPDATE

type Msg
  = UpdateRed Int Bool
  | UpdateYellow Int Bool
  | UpdateGreen Int Bool
  | UpdateBlue Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateRed idx isChecked ->
      (
        { model | redCheckedStates = Array.set idx isChecked model.redCheckedStates }
        , Cmd.none
      )
    UpdateYellow idx isChecked ->
      (
        { model | yellowCheckedStates = Array.set idx isChecked model.yellowCheckedStates }
        , Cmd.none
      )
    UpdateGreen idx isChecked ->
      (
        { model | greenCheckedStates = Array.set idx isChecked model.greenCheckedStates }
        , Cmd.none
      )
    UpdateBlue idx isChecked ->
      (
        { model | blueCheckedStates = Array.set idx isChecked model.blueCheckedStates }
        , Cmd.none
      )

-- VIEW
rowBox: (Int, Bool) -> Html Msg
rowBox (idx, isChecked) =
    div 
      [ class "qwixx-box", onClick (UpdateRed idx (not isChecked)) ]
      [ div [ 
          classList [ ("checked", isChecked), ("not-checked", not isChecked) ]
        ]
        [ text "X" ]
      , div [ class "qwixx-value" ] [ text (String.fromInt (idx + 2)) ]
      ]

row: Array Bool -> String -> Html Msg
row rowArray color =
    div [ class "qwixx-row", class color ]
        (List.map rowBox (Array.toIndexedList rowArray))

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
      ]
    ]
