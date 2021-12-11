module Main exposing (..)

import Dict exposing (Dict)
import Browser
import Html exposing (Html)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Events.Extra exposing (onEnter)

import Json.Encode as E
import Json.Decode as D
import Json.Decode.Extra as DExtra

import Ports exposing (setStorage)


type alias Model =
    { tasks: Dict Int Task -- Lists of tasks by IDs
    , next_task_id: Int -- Next ID to use for a new task
    , next_task_text: String -- Text for the next task
    }

encode : Model -> E.Value
encode model = 
    E.object
        [ ("tasks", E.dict String.fromInt encodeTask model.tasks)
        , ("next_task_id", E.int model.next_task_id)
        , ("next_task_text", E.string model.next_task_text)
        ]

decode : D.Decoder Model
decode =
    D.map3 Model
        (D.field "tasks" (DExtra.dict2 D.int decodeTask))
        (D.field "next_task_id" D.int)
        (D.field "next_task_text" D.string)

type alias Task =
    { text: String
    , checked: Bool
    }

encodeTask: Task -> E.Value
encodeTask { text, checked } =
    E.object
        [ ("text", E.string text)
        , ("checked", E.bool checked)
        ]

decodeTask : D.Decoder Task
decodeTask =
    D.map2 Task
        (D.field "text" D.string)
        (D.field "checked" D.bool)

type Msg = AddTask Task | TypedNextTask String | CheckTask Int Bool | RemoveTask Int

main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : D.Value -> (Model, Cmd Msg)
init encoded =
    case D.decodeValue decode encoded of
        Ok model -> (model, Cmd.none)

        Err e -> Debug.log "Error" e |> \_ -> ({ tasks = Dict.empty, next_task_id = 0, next_task_text = "" }, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        AddTask task ->
            -- Check that task text is not empty
            if task.text == "" then (model, Cmd.none) else
                -- Add the task to the list of tasks
                let tasks = Dict.insert model.next_task_id task model.tasks
                    next_task_id = model.next_task_id - 1 -- -1 so tasks are added from the top
                in
                { model | tasks = tasks, next_task_id = next_task_id, next_task_text = "" } |> modelAndSave

        TypedNextTask task -> { model | next_task_text = task } |> modelAndSave

        CheckTask index checked ->
            case model.tasks |> Dict.get index of
                Just task ->
                    { model | tasks = Dict.insert index { task | checked = checked } model.tasks } |> modelAndSave

                Nothing -> (model, Cmd.none)

        RemoveTask index ->
            { model | tasks = Dict.remove index model.tasks } |> modelAndSave

modelAndSave : Model -> (Model, Cmd Msg)
modelAndSave model = (model, model |> encode |> setStorage)

colors =
    { dark = El.rgb255 25 28 26
    , light = El.rgb255 226 233 225
    , saturated = El.rgb255 48 164 60
    }

view : Model -> Html Msg
view model =
    viewMainPanel model |> El.layout []

viewMainPanel : Model -> Element Msg
viewMainPanel model =
    El.column
        [ El.centerX
        , El.width El.fill, El.height El.fill
        , El.spacing 30
        , El.padding 20
        , Background.color colors.dark
        ]
        [ viewAddTask model
        , viewTasks model.tasks
        ]

viewAddTask : Model -> Element Msg
viewAddTask model =
    El.el
        [ El.centerX
        , Background.color colors.light
        , Border.rounded 50
        ]
    <| El.row [ El.padding 20 ]
        [ Input.text
            [ El.htmlAttribute <| onEnter (AddTask <| { text = model.next_task_text, checked = False })
            ]
            { text = model.next_task_text
            , onChange = TypedNextTask
            , placeholder = Nothing
            , label = Input.labelLeft [] (El.text "New Task")
            }
        ]

viewTasks : Dict Int Task -> Element Msg
viewTasks tasks =
    El.column 
        [ El.centerX
        , El.spacing 20
        ]
        (tasks |> Dict.map viewTask |> Dict.toList |> List.map (\(_, b) -> b))

viewTask : Int -> Task -> Element Msg
viewTask index task =
    El.row
        [ Border.color colors.saturated
        , Border.width 5
        , Border.rounded 5
        , El.padding 5
        , El.spacing 5
        , Background.color colors.light
        , Font.size 26
        ]
        [ Input.checkbox []
            { checked = task.checked
            , onChange = CheckTask index
            , icon = Input.defaultCheckbox
            , label = Input.labelHidden "Done"
            }
        , El.text task.text
        , Input.button []
            { onPress = Just (RemoveTask index)
            , label = El.el [Font.size 10] (El.text "X")
            }
        ]

