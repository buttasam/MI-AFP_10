import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Debug exposing (log)
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

apiToken = "9ee063ac99fb10c0d618f4e52ae1a30a91e4ef6f"

type alias Currency =
  {
    code : String,
    name : String,
    sign : String
  }

type alias Exchange =
  {
    base : String,
    rates : List (String, Float)
  }

type alias Model =
  {
    currencies : List Currency,
    exchanges : List Exchange
  }

model : Model
model = Model [] []

init : (Model, Cmd Msg)
init = (model, getCurrenciesData)

-- UPDATE
type Msg = Tick Time
         | NewExchange (Result Http.Error Exchange)
         | NewCurrencies (Result Http.Error (List Currency))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      (log "ticked")
      (model, getCurrenciesData)
    NewCurrencies (Ok data) ->
      (log (toString data))
      ( { model | currencies = data }, Cmd.none)
    NewCurrencies (Err e) ->
      (log (toString e))
      (model, Cmd.none)
    NewExchange (Ok data) ->
      (log (toString data))
      (model, Cmd.none)
    NewExchange (Err e) ->
      (log (toString e))
      (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    fieldset [] (generateInupts model.currencies)


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


-- HTTP
getCurrenciesData : Cmd Msg
getCurrenciesData =
  let
    url = "https://dummy-currency-api.herokuapp.com/currencies"
    request =
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Authorization" (" token " ++ apiToken) ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson currenciesDecoder
                , timeout = Nothing
                , withCredentials = False
                }
  in
    Http.send NewCurrencies request

-- HTTP
getExchangeData : String -> Cmd Msg
getExchangeData code =
  let
    url = "https://dummy-currency-api.herokuapp.com/exchange-rates/" ++ code
    request =
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Authorization" (" token " ++ apiToken) ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson exchangeDecoder
                , timeout = Nothing
                , withCredentials = False
                }
  in
    Http.send NewExchange request

currenciesDecoder : Decoder (List Currency)
currenciesDecoder =
    Decode.list currencyDecoder

currencyDecoder : Decoder Currency
currencyDecoder =
  Pipeline.decode Currency
    |> Pipeline.required "code" string
    |> Pipeline.required "name" string
    |> Pipeline.required "sign" string

exchangeDecoder : Decoder Exchange
exchangeDecoder =
  Pipeline.decode Exchange
    |> Pipeline.required "base" string
    |> Pipeline.required "rates" (keyValuePairs float)
