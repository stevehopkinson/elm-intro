import Html exposing (..)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import DieFaces

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { dieFace : Int,
    dieFace2 : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 1 1, Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NewFace Int
  | NewFace2 Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Cmd.batch [Random.generate NewFace (Random.int 1 6), Random.generate NewFace2 (Random.int 1 6)])

    NewFace newFace ->
      ({ model | dieFace = newFace }, Cmd.none)

    NewFace2 newFace ->
      ({ model | dieFace2 = newFace }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ dieFace model.dieFace
    , dieFace model.dieFace2
    , button [ onClick Roll ] [ Html.text "Roll" ]
    ]

faceMapper : (Float, Float) -> Svg Msg
faceMapper (x, y) =
  circle
    [ cx (toString x)
    , cy (toString y)
    , r "0.07"
    ] []

dieFace : Int -> Svg Msg
dieFace face =
  svg
    [ width "100"
    , height "100"
    , viewBox "0 0 1 1"
    , Svg.Attributes.style "border: 3px solid; border-color: black; border-radius: 15px; margin: 5px"
    ]
    ( List.map faceMapper (DieFaces.dotLocations face) )
