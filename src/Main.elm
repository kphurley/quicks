module Main exposing (..)

-- Render an interactive Qwixx scoresheet.
--

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



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


type alias Model =
    { redCheckedStates : Array Bool
    , yellowCheckedStates : Array Bool
    , greenCheckedStates : Array Bool
    , blueCheckedStates : Array Bool
    , penaltyStates : Array Bool
    }


init : () -> ( Model, Cmd msg )
init () =
    ( { redCheckedStates = Array.initialize 12 (always False)
      , yellowCheckedStates = Array.initialize 12 (always False)
      , greenCheckedStates = Array.initialize 12 (always False)
      , blueCheckedStates = Array.initialize 12 (always False)
      , penaltyStates = Array.initialize 4 (always False)
      }
    , Cmd.none
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
                    ( { model | redCheckedStates = Array.set idx isChecked model.redCheckedStates }
                    , Cmd.none
                    )

                "yellow" ->
                    ( { model | yellowCheckedStates = Array.set idx isChecked model.yellowCheckedStates }
                    , Cmd.none
                    )

                "green" ->
                    ( { model | greenCheckedStates = Array.set idx isChecked model.greenCheckedStates }
                    , Cmd.none
                    )

                "blue" ->
                    ( { model | blueCheckedStates = Array.set idx isChecked model.blueCheckedStates }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


getValueElement : Int -> Html Msg
getValueElement idx =
    if idx == 11 then
        img
            [ class "lock"
            , src "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Ei-lock.svg/512px-Ei-lock.svg.png"
            ]
            []

    else
        text (String.fromInt (idx + 2))



-- VIEW


rowBox : ( Int, Bool, String ) -> Html Msg
rowBox ( idx, isChecked, color ) =
    div
        [ class "qwixx-box", onClick (UpdateCheckedState idx (not isChecked) color) ]
        [ div
            [ classList [ ( "checked", isChecked ), ( "not-checked", not isChecked ) ]
            ]
            [ text "X" ]
        , div [ class "qwixx-value" ] [ getValueElement idx ]
        ]


rowMapper : ( Int, Bool ) -> String -> ( Int, Bool, String )
rowMapper ( idx, b ) str =
    ( idx, b, str )


row : Array Bool -> String -> Html Msg
row rowArray color =
    div [ class "qwixx-row", class color ]
        (List.map rowBox (List.map (\tup -> rowMapper tup color) (Array.toIndexedList rowArray)))


reversedIndexList : Array Bool -> List ( Int, Bool )
reversedIndexList array =
    List.reverse (Array.toIndexedList array)


reversedWithLock : List ( Int, Bool ) -> List ( Int, Bool )
reversedWithLock list =
    List.drop 1 list ++ List.take 1 list


rowReversed : Array Bool -> String -> Html Msg
rowReversed rowArray color =
    div [ class "qwixx-row", class color ]
        (List.map rowBox (List.map (\tup -> rowMapper tup color) (reversedWithLock (reversedIndexList rowArray))))


scoreMap : Int -> Int
scoreMap numberOfChecks =
    List.sum (List.range 0 numberOfChecks)


computeScore : Model -> String -> Int
computeScore model color =
    case color of
        "red" ->
            scoreMap (Array.length (Array.filter identity model.redCheckedStates))

        "yellow" ->
            scoreMap (Array.length (Array.filter identity model.yellowCheckedStates))

        "green" ->
            scoreMap (Array.length (Array.filter identity model.greenCheckedStates))

        "blue" ->
            scoreMap (Array.length (Array.filter identity model.blueCheckedStates))

        -- to handle : "penalty"
        _ ->
            0


sumScores : Model -> Int
sumScores model =
    computeScore model "red"
        + computeScore model "yellow"
        + computeScore model "green"
        + computeScore model "blue"
        - computeScore model "penalty"


scoreRow : Model -> Html Msg
scoreRow model =
    div
        [ class "qwixx-row" ]
        [ div [ class "qwixx-box red-border" ]
            [ div [ class "qwixx-value" ] [ text (String.fromInt (computeScore model "red")) ]
            ]
        , div [ class "operator" ] [ text "+" ]
        , div [ class "qwixx-box yellow-border" ]
            [ div [ class "qwixx-value" ] [ text (String.fromInt (computeScore model "yellow")) ]
            ]
        , div [ class "operator" ] [ text "+" ]
        , div [ class "qwixx-box green-border" ]
            [ div [ class "qwixx-value" ] [ text (String.fromInt (computeScore model "green")) ]
            ]
        , div [ class "operator" ] [ text "+" ]
        , div [ class "qwixx-box blue-border" ]
            [ div [ class "qwixx-value" ] [ text (String.fromInt (computeScore model "blue")) ]
            ]
        , div [ class "operator" ] [ text "-" ]
        , div [ class "qwixx-box black-border" ]
            [ div [ class "qwixx-value" ] [ text (String.fromInt (computeScore model "penalty")) ]
            ]
        , div [ class "operator" ] [ text "=" ]
        , div [ class "qwixx-box" ]
            [ div [ class "qwixx-final-value" ] [ text (String.fromInt (sumScores model)) ]
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ class "qwixx-app" ]
        [ div
            [ class "qwixx-container" ]
            [ row model.redCheckedStates "red"
            , row model.yellowCheckedStates "yellow"
            , rowReversed model.greenCheckedStates "green"
            , rowReversed model.blueCheckedStates "blue"
            , row model.penaltyStates "black"
            , scoreRow model
            ]
        ]
