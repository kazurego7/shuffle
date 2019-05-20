module Main exposing (Model, Msg(..), init, main, nDieView, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Random.Extra
import Random.List



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { range : ( Int, Int )
    , n : Int
    , nDieFaces : List (List Int)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { range = ( 0, 6 )
      , n = 2
      , nDieFaces = List.repeat 2 (List.range 0 6)
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFaces (List (List Int))
    | Change Int



-- sequence : List (Random.Generator a) -> Random.Generator (List a)
-- sequence =
--     List.foldr (Random.map2 (::)) (Random.constant [])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            let
                ( begin, end ) =
                    model.range

                shuffled : Random.Generator (List (List Int))
                shuffled =
                    List.repeat model.n (List.range begin end)
                        |> List.map Random.List.shuffle
                        |> Random.Extra.sequence
            in
            ( model
            , Random.generate NewFaces shuffled
            )

        NewFaces newFaces ->
            ( { model
                | nDieFaces = newFaces
              }
            , Cmd.none
            )

        Change n ->
            ( { model | n = n }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


nDieView : Model -> List (Html msg)
nDieView model =
    let
        toTexts : List Int -> Html msg
        toTexts list =
            text (String.append (String.concat (List.map String.fromInt list)) "\n")
    in
    List.map toTexts model.nDieFaces


view : Model -> Html Msg
view model =
    let
        toChange : String.String -> Msg
        toChange =
            String.toInt >> Maybe.withDefault 0 >> Change
    in
    div []
        [ input [ placeholder "n", value (String.fromInt model.n), onInput toChange ] []
        , button [ onClick Roll ] [ text "Roll" ]
        , h1 [] (nDieView model)
        ]
