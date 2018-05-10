import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Debug exposing (log)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Currency =
  {
    code : String,
    name : String,
    sign : String
  }

type alias Model = Int

model : Model
model =
  0

init : (Model, Cmd Msg)
init = (model, Cmd.none)

dummyCurrencies : List (Currency)
dummyCurrencies = [Currency "USD" "Dollar" "$", Currency "EUR" "Euro" "€"]

-- UPDATE

type Msg = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      (log "ticked")
      (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    fieldset [] (generateInupts dummyCurrencies)


generateInput : Currency -> Html msg
generateInput c =
  label []
      [ input [ type_ "number" ] []
      , text c.name
      , br [] []
      ]

generateInupts : List (Currency) -> List (Html msg)
generateInupts currencies =
  List.map generateInput currencies

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (5 * second) Tick
