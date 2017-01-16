import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Regex exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , validated : Html Msg
  }

model : Model

model =
  Model "" "" "" "" (div [] [])

-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Validate

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    Password password ->
      { model | password = password }
    PasswordAgain password ->
      { model | passwordAgain = password }
    Age age ->
      { model | age = age }
    Validate ->
      { model | validated = viewValidation model }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter password", onInput PasswordAgain ] []
    , input [ type_ "number", placeholder "Age", onInput Age ] []
    , model.validated
    , button [ onClick Validate ] [ text "Submit" ]
    ]

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if String.length model.password < 8 then
        ("orange", "Passwords must be at least six characters")
      else if model.password /= model.passwordAgain then
        ("red", "Passwords do not match")
      else if List.length (Regex.find All (Regex.regex "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)[A-Za-z\\d!$%@#£€*?&]{8,}$") model.password) < 1 then
        ("red", "Password must contain a capital letter, a lower-case letter and a number")
      else if not (validateAgeString model.age) then
        ("red", "Age must be an integer")
      else
        ("green", "OK!")
  in
    div [ style [("color", color)] ] [ text message ]

validateAgeString : String -> Bool
validateAgeString stringAge =
  case String.toInt stringAge of
    Ok age ->
      True
    Err _ ->
      False
