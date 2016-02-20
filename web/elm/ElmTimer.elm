module ElmTimer where


import StartApp
import Effects
import Task
import String
import Html exposing (..)
import Html.Attributes exposing (class)
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


init =
  { runners =
      [ { first_name = "Jarkko", last_name = "Laine", id = "abcde" }
      , { first_name = "Tarmo", last_name = "Tänav", id = "abcdef" }
      ]
  , results =
      [ { runner_id = "abcde", time = 12345 }
      , { runner_id = "abcdef", time = 12347 }
      ]
  , timer = { start_time = Nothing }
  , current_time = 0
  }

app : StartApp.App Model
app = StartApp.start
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [Signal.map (\t -> Tick t) (every Time.second)]}

main : Signal Html
main = app.html


type alias Timer =
  { start_time : Maybe Time.Time
  }


type alias Runner =
  { first_name : String
  , last_name : String
  , id : String
  }


type alias Result =
  { runner_id : String
  , time : Time.Time
  }

type alias Model =
  { runners : List Runner
  , timer : Timer
  , results : List Result
  , current_time : Time.Time
  }


-- UPDATE

type Action = Tick Time.Time | Start

update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Tick t ->
      let
        new_model = { model | current_time = t }
      in
        (new_model, Effects.none)
    Start ->
      let
        new_model = { model | timer = { start_time = Just model.current_time }}
      in
        (new_model, Effects.none)

-- VIEW

paddedTime : String -> String
paddedTime t =
  String.padLeft 2 '0' t


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

timer : Model -> Html
timer model =
  if model.timer.start_time == Nothing then
    div [ class "clock" ] [ text "Not started" ]
  else
    div [ class "clock" ]
        [ currentTime (model.current_time - withDefault 0 model.timer.start_time) ]

view : Address Action -> Model -> Html
view address model =
  div [ class "main" ]
      [ div [ class "clock" ] [ currentTime model.current_time ]
      , timer model
      , ul [ class "results" ] (List.map (resultItem model.runners) model.results)
      , div [] [ button [ onClick address Start ] [ text "Start Timer" ] ]
      ]


resultItem : List Runner -> Result -> Html
resultItem runners result =
  li [ class "result" ]
     [ text (nameFor result.runner_id runners)
     , text (", ")
     , text (toString result.time)
     ]

nameFor : String -> List Runner -> String
nameFor uuid runners =
  List.filter (\r -> r.id == uuid && (String.length(uuid) > 0)) runners
  |> List.head
  |> Maybe.withDefault ({ id = "123", first_name = "Foo", last_name = "Bar" })
  |> (\r -> r.first_name ++ " " ++ r.last_name)
