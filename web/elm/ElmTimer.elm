module ElmTimer where


import TimeApp
import Effects
-- import Task
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Uuid
import Random.PCG exposing (Seed, initialSeed2, generate)
import Signal exposing (Address)
import Time exposing (every, second)
import Date exposing (year, hour, minute, second, fromTime)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)

port randomSeed : (Int, Int)


seed0 : Seed
seed0 =
  (uncurry initialSeed2) randomSeed


init : Model
init =
  { runners =
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
  }


app : TimeApp.App Model
app = TimeApp.start
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [Signal.map (\t -> Tick t) (every (Time.second / 5))]}


main : Signal Html
main = app.html


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


type Action
  = Tick Time.Time
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
      |> List.filter (\r -> r.bib_number == bib_number && (String.length(bib_number) > 0))
      |> List.map (\r -> r.id)
  in
    if List.length ids > 0 then
      List.head ids
    else
      Nothing


update : Action -> Time.Time -> Model -> ( Model, Effects.Effects Action )
update action now model =
  case action of
    Tick t ->
      let
        new_model = { model | current_time = now }
      in
        (new_model, Effects.none)
    Start ->
      let
        new_model = { model | timer = { start_time = Just now }}
      in
        (new_model, Effects.none)
    Stop ->
      let
        new_model = { model | timer = { start_time = Nothing }}
      in
        (new_model, Effects.none)
    StoreResult ->
      let
        res_time = now - (Maybe.withDefault 0 model.timer.start_time)
        (newUuid, newSeed) = generate Uuid.uuidGenerator model.current_seed
        new_result = { runner_id = Nothing, time = res_time, uuid = newUuid, bib_number = "" }
        results = List.append model.results [new_result]
        new_model = { model | results = results, current_seed = newSeed }
      in
        (new_model, Effects.none)
    UpdateResult id bib_number ->
      let
        updateResult t =
          if t.uuid == id then
            { t | bib_number = bib_number }
          else t
        newModel = { model | results = List.map updateResult model.results }
      in
        (newModel, Effects.none)
    EditingResult id isEditing ->
      let
        updateResult t =
          if t.uuid == id then
            { t | runner_id = runnerIdByBib t.bib_number model.runners }
          else t
        newModel = if (not isEditing) then
          { model | results = List.map updateResult model.results }
        else
          model
      in
        (newModel, Effects.none)

-- VIEW

onEnter : Address a -> a -> Attribute
onEnter address value =
  on "keydown"
    (Json.customDecoder keyCode is13)
    (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


paddedTime : String -> String
paddedTime t =
  String.padLeft 2 '0' t


formattedTimeInterval : Time.Time -> String
formattedTimeInterval t =
  let
    secs = (floor (Time.inSeconds t)) % 60
    minutes = (floor (Time.inMinutes t)) % 60
    hours = floor (Time.inHours t)
    times = [hours, minutes, secs]
  in
    times
    |> List.map toString
    |> List.map paddedTime
    |> String.join ":"


currentTime : Time.Time -> Html
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


timeDiff : Time.Time -> Time.Time -> Html
timeDiff to from =
  let
    diff = to - from
  in
    formattedTimeInterval (if diff > 0 then diff else 0)
    |> text


timer : Model -> Html
timer model =
  if model.timer.start_time == Nothing then
    div [ class "clock" ] [ text "Not started" ]
  else
    div [ class "clock" ]
        [ timeDiff model.current_time (withDefault 0 model.timer.start_time) ]


results : Address Action -> Model -> Html
results address model =
  let
    res = List.map (resultItem address model.runners) model.results
  in
    ol [ class "results" ] res


startStopButton : Address Action -> Model -> Html
startStopButton address model =
  if model.timer.start_time == Nothing then
    div [] [ button [ onClick address Start ] [ text "Start Timer" ] ]
  else
    div [] [ button [ onClick address Stop ] [ text "Stop Timer" ] ]


view : Address Action -> Model -> Html
view address model =
  div [ class "main" ]
      [ div [ class "clock" ] [ currentTime model.current_time ]
      , timer model
      , results address model
      , startStopButton address model
      , storeResultDiv address model
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


bibNumberField : Address Action -> TimerResult -> List Runner -> Html
bibNumberField address result runners =
  let
    -- number = bibNumberFor result.runner_id runners
    number = result.bib_number
  in
    input
      [ class "edit"
      , value number
      , id ("result-" ++ Uuid.toString(result.uuid))
      , on "input" targetValue (Signal.message address << UpdateResult result.uuid)
      , onBlur address (EditingResult result.uuid False)
      , onEnter address (EditingResult result.uuid False)
      ]
      [ ]


resultItem : Address Action -> List Runner -> TimerResult -> Html
resultItem address runners result =
  li [ class "result" ]
     [ bibNumberField address result runners
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


storeResultDiv : Address Action -> Model -> Html
storeResultDiv address model =
  div [] [ button [ onClick address StoreResult ] [ text "Store Result" ] ]
