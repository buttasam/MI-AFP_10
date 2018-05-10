import Html exposing (Html, beginnerProgram, fieldset, input, label, text, br)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model = Int

model : Model
model =
  0


-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

view : Model -> Html Msg
view model =
    fieldset []
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
