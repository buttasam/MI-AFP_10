import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = Int

model : Model
model =
  0

init : (Model, Cmd Msg)
init = (model, Cmd.none)


-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      (model + 1, Cmd.none)

    Decrement ->
      (model - 1, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    fieldset [] (generateInupts "")


generateInupts : String -> List (Html msg)
generateInupts s =
  [ label []
      [ input [ type_ "text" ] []
      , text "USD"
      ]
    , br [] []
    ,label []
        [ input [ type_ "text" ] []
        , text "EUR"
        ]
    , br [] []
    ,label []
      [ input [ type_ "text" ] []
      , text "CZK"
      ]
    , br [] []
    ,label []
      [ input [ type_ "text" ] []
      , text "GBP"
      ]
    , br [] []
    ,label []
      [ input [ type_ "text" ] []
      , text "CNY"
      ]
  ]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
