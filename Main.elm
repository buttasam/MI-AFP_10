import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Debug exposing (log)
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import List.Extra exposing (find)
import Tuple exposing (first, second)


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
    firstValue : Float,
    synchData : Bool,
    currencies : List Currency,
    firstExchange : Exchange
  }

model : Model
model = Model 1 True [] (Exchange "USD" [])

init : (Model, Cmd Msg)
init = (model, getCurrenciesData)

-- UPDATE
type Msg = Tick Time
         | UpdateSynch
         | KeyInput String String
         | NewExchange (Result Http.Error Exchange)
         | NewCurrencies (Result Http.Error (List Currency))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    code = model.firstExchange.base
  in
  case msg of
    Tick time ->
      (model, getCurrenciesData)
    KeyInput code data ->
      ({ model | firstValue = (calculateFirstValue data code model) }, Cmd.none)
    UpdateSynch ->
      ({ model | synchData = not model.synchData }, Cmd.none)
    NewCurrencies (Ok data) ->
      ( { model | currencies = data }, getExchangeData code)
    NewCurrencies (Err e) ->
      (toString e |> log)
      (model, Cmd.none)
    NewExchange (Ok data) ->
      ( { model | firstExchange = data }, Cmd.none)
    NewExchange (Err e) ->
      (toString e |> log)
      (model, Cmd.none)

calculateFirstValue : String -> String -> Model -> Float
calculateFirstValue data code model =
  case String.toFloat data of
    Err _ ->
      log "error"
      0
    Ok value ->
      let
        exchange = (find (\rate -> (first rate) == code ) model.firstExchange.rates)
      in
        case exchange of
          Nothing ->
            value
          Just v ->
            value / (Tuple.second v)

-- VIEW
view : Model -> Html Msg
view model =
    fieldset [] (List.append (generateInupts model) (generateRestHtml model.synchData))


generateInupts : Model -> List (Html Msg)
generateInupts model =
  let
    currencies = model.currencies
    generateInput : Currency -> Html Msg
    generateInput c =
      label []
          [ input [ type_ "number", onInput (KeyInput c.code) , Html.Attributes.value (calculateRate c model |> toString )] []
          , text c.name
          , br [] []
          ]
  in
    List.map generateInput currencies

calculateRate : Currency -> Model -> Float
calculateRate currency model =
  let
    exchange = (find (\rate -> (first rate) == currency.code ) model.firstExchange.rates)
  in
    case exchange of
      Nothing ->
        model.firstValue
      Just v ->
        model.firstValue * (Tuple.second v)


generateRestHtml : Bool -> List (Html Msg)
generateRestHtml synchData = [
      button [ onClick UpdateSynch ] [ text ( if synchData then "Stop" else "Start") ]
    ]

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  if model.synchData then Time.every (5 * Time.second) Tick else Sub.none


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
