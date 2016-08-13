port module ElmTimer exposing (..)

-- import Task
import String
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)
import Json.Decode as Json
import Uuid
import Random.Pcg exposing (Seed, initialSeed2, step)
import Time exposing (Time, every, second)
import Date exposing (year, hour, minute, second, fromTime)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Task.Extra

port randomSeed : ((Int, Int) -> msg) -> Sub msg


seed0 : Seed
seed0 =
  (uncurry initialSeed2) randomSeed

initialUuid : (Uuid.Uuid, Seed)
initialUuid =
  Random.Pcg.step Uuid.uuidGenerator seed0

init : (Model, Cmd Msg)
init =
  ({ runners =
    [
      { first_name = "Jarkko", last_name = "Laine", id = "abcde", bib_number = "2" }
    , { first_name = "Tarmo", last_name = "Tänav", id = "abcdef", bib_number = "3" }
    ]
  , results =
    []
  , timer = { start_time = Nothing }
  , current_time = 0
  , current_seed = seed0
  , current_uuid = Nothing
  }, Cmd.none)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Timer =
  { start_time : Maybe Time.Time
  }


type alias Runner =
  { first_name : String
  , last_name : String
  , id : String
  , bib_number: String
  }


type alias TimerResult =
  { runner_id : Maybe String
  , uuid : Uuid.Uuid
  , time : Time.Time
  , bib_number : String
  , editing : Bool
  }


type alias Model =
  { runners : List Runner
  , timer : Timer
  , results : List TimerResult
  , current_time : Time.Time
  , current_seed : Seed
  , current_uuid : Maybe Uuid.Uuid
  }


-- UPDATE


type Msg
  = Tick Time
  | NoOp
  | Start
  | Stop
  | StoreResult
  | UpdateResult Uuid.Uuid String
  | EditingResult Uuid.Uuid Bool


runnerIdByBib : String -> List Runner -> Maybe String
runnerIdByBib bib_number runners =
  let
    ids =
      runners
      |> List.filter (\r -> r.bib_number == bib_number &&
                            (String.length(bib_number) > 0))
      |> List.map (\r -> r.id)
  in
    if List.length ids > 0 then
      List.head ids
    else
      Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    Tick t ->
      let
        new_model = { model | current_time = t }
      in
        (new_model, Cmd.none)
    Start ->
      let
        start_time = Just model.current_time
        new_model = { model | timer = { start_time = start_time }}
      in
        (new_model, Cmd.none)
    Stop ->
      let
        new_model = { model | timer = { start_time = Nothing }}
      in
        (new_model, Cmd.none)
    StoreResult ->
      let
        -- effects = sendTask |> Task.Extra.performFailproof identity
        -- figure out how to do the outgoing port stuff
        effects = Cmd.none

        res_time = model.current_time - (Maybe.withDefault 0 model.timer.start_time)
        (newUuid, newSeed) = Random.Pcg.step Uuid.uuidGenerator model.current_seed
        new_result =
          { runner_id = Nothing
          , time = res_time
          , uuid = newUuid
          , bib_number = ""
          , editing = True }
        results = List.append model.results [new_result]
        new_model = { model | results = results, current_seed = newSeed }
      in
        if model.timer.start_time == Nothing then
          (model, effects)
        else
          (new_model, effects)
    UpdateResult id bib_number ->
      let
        updateResult t =
          if t.uuid == id then
            { t | bib_number = bib_number }
          else t
        newModel = { model | results = List.map updateResult model.results }
      in
        (newModel, Cmd.none)
    EditingResult id isEditing ->
      let
        updateResult t =
          if t.uuid == id then
            { t | runner_id = runnerIdByBib t.bib_number model.runners
                , editing = isEditing }
          else t
        newModel = { model | results = List.map updateResult model.results }
      in
        (newModel, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second Tick


-- VIEW

onEnter : Attribute ()
onEnter =
  onWithOptions
    "keydown"
    {preventDefault = True, stopPropagation = True}
    (Json.customDecoder keyCode is13)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


paddedTime : String -> String
paddedTime t =
  String.padLeft 2 '0' t


formattedTimeInterval : Time.Time -> Bool -> String
formattedTimeInterval t parts =
  let
    secs = ((floor (Time.inSeconds t)) % 60) |> toString
    minutes = ((floor (Time.inMinutes t)) % 60) |> toString
    hours = (floor (Time.inHours t)) |> toString
    times = [hours, minutes, secs]
  in
    times
    |> List.map paddedTime
    |> String.join ":"


currentTime : Time.Time -> Html Msg
currentTime t =
  let date' = fromTime t
      hour' = toString (Date.hour date')
      minute' = toString (Date.minute date')
      second' = toString (Date.second date')
      year' = toString (year date')
      now = "The current time is: "
        ++ (paddedTime hour') ++ ":"
        ++ (paddedTime minute') ++ ":"
        ++ (paddedTime second')
  in
      text now


timeDiff : Time.Time -> Time.Time -> Html Msg
timeDiff to from =
  let
    diff = to - from
  in
    formattedTimeInterval (if diff > 0 then diff else 0)
    |> toString
    |> text


timer : Model -> Html Msg
timer model =
  if model.timer.start_time == Nothing then
    div [ class "clock" ] [ text "Not started" ]
  else
    div [ class "clock" ]
        [ timeDiff model.current_time (withDefault 0 model.timer.start_time) ]


results : Model -> Html Msg
results model =
  let
    res = List.map (resultItem model.runners) model.results
  in
    ol [ class "results" ] res


startStopButton : Model -> Html Msg
startStopButton model =
  if model.timer.start_time == Nothing then
    div [] [ button [ onClick Start ] [ text "Start Timer" ] ]
  else
    div [] [ ]


view : Model -> Html Msg
view model =
  div [ class "main" ]
      [ div [ class "clock" ] [ currentTime model.current_time ]
      , timer model
      , results model
      , startStopButton model
      , storeResultDiv model
      ]


bibNumberFor : Maybe String -> List Runner -> String
bibNumberFor runner_id runners =
  if runner_id == Nothing then
    ""
  else
    runners
    |> List.filter (\r -> r.id == (Maybe.withDefault "" runner_id))
    |> List.head
    |> Maybe.withDefault ({ id = "123", first_name = "Foo", last_name = "Bar", bib_number = "1" })
    |> (\r -> r.bib_number)


bibNumberField : TimerResult -> List Runner -> Html Msg
bibNumberField result runners =
  let
    -- number = bibNumberFor result.runner_id runners
    number = result.bib_number
  in
    input
      [ class "edit"
      , value number
      , id ("result-" ++ Uuid.toString(result.uuid))
      , onInput (UpdateResult result.uuid)
      , onBlur (EditingResult result.uuid False)
      , onEnter (EditingResult result.uuid False)
      ]
      [ ]

bibNumberDiv : TimerResult -> List Runner -> Html Msg
bibNumberDiv result runners =
  let
    number = result.bib_number
  in
    if result.editing || result.runner_id == Nothing then
      bibNumberField result runners
    else
      span
        [ class "edit"
        , id ("result-" ++ Uuid.toString(result.uuid))
        , onClick (EditingResult result.uuid True)
        ]
        [ text number ]


resultItem : List Runner -> TimerResult -> Html Msg
resultItem runners result =
  -- Debug.log(toString result)
  li [ class "result" ]
     [ bibNumberDiv result runners
     , text (nameFor (Maybe.withDefault "" result.runner_id) runners)
     , text (", ")
     , text (formattedTimeInterval result.time)
     ]


nameFor : String -> List Runner -> String
nameFor uuid runners =
  runners
  |> List.filter (\r -> r.id == uuid && (String.length(uuid) > 0))
  |> List.head
  |> Maybe.withDefault ({ id = "123", first_name = "Unknown", last_name = "Runner", bib_number = "1" })
  |> (\r -> r.first_name ++ " " ++ r.last_name)


storeResultDiv : Model -> Html Msg
storeResultDiv model =
  div []
      [ button
        [ id "store-result-button"
        , onWithOptions "click" {preventDefault = True, stopPropagation = True}
            (Json.value)
            StoreResult
        ]
        [ text "Store Result" ]
      ]
