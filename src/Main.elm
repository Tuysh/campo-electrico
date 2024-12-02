module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Browser.Dom exposing (getViewport)
import Element as E exposing (fill, centerX)
import Element.Background as Background
import Html exposing (Html)
import Html.Attributes exposing (style)
import Math.Vector2 as Vector2 exposing (vec2)
import Simulation exposing (Model, Msg, Position, Sign)
import Task
import Utils exposing (toElmUiColor)

type alias Model =
    { activeSimulation : Simulation.Model
    , cursor : Cursor
    , isMouseDown : Bool
    , isInteractionEnabled : Bool
    }

type Cursor
    = Selector
    | Painter Sign
    | Deleter

type Msg
    = SimulationMsg Simulation.Msg
    | GotViewport Browser.Dom.Viewport
    | WindowResized Int Int
    | UpdateActiveSimulationState
    | UpdateCursor Cursor
    | DrawCharges Position
    | CursorClicked Position
    | MouseDown
    | MouseUp

init : ( Model, Cmd Msg )
init =
    ( { activeSimulation = Simulation.init 1200 750
      , cursor = Selector
      , isMouseDown = False
      , isInteractionEnabled = True
      }
    , Task.perform GotViewport getViewport
    )

view : Model -> Html Msg
view model =
    E.layout
        [ E.width fill
        , E.height fill
        , Background.color <| toElmUiColor model.activeSimulation.settings.colors.background
        , E.htmlAttribute <| style "touch-action" "none"
        ]
        (E.el [ centerX ]
            (E.html (Html.map SimulationMsg <| Simulation.view model.activeSimulation))
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SimulationMsg msg ->
            let
                ( newSimulation, cmd ) =
                    Simulation.update msg model.activeSimulation
            in
            ( { model | activeSimulation = newSimulation }
            , Cmd.map SimulationMsg cmd
            )

        GotViewport viewport ->
            ( updateSimulationSize viewport.viewport.width viewport.viewport.height model
            , Cmd.none
            )

        WindowResized width height ->
            ( updateSimulationSize (toFloat width) (toFloat height) model
            , Cmd.none
            )

        UpdateActiveSimulationState ->
            ( { model 
                | activeSimulation =
                    let 
                        sim = model.activeSimulation
                        nextState = 
                            case sim.state of
                                Simulation.Running -> Simulation.Resting
                                Simulation.Resting -> Simulation.Running
                    in
                    { sim | state = nextState }
              }
            , Cmd.none
            )

        UpdateCursor newCursor ->
            ( { model 
                | cursor = newCursor
                , isInteractionEnabled = newCursor /= Deleter 
              }
            , Cmd.none
            )

        DrawCharges position ->
            ( case model.cursor of
                Painter sign ->
                    if model.isMouseDown then
                        { model | activeSimulation = Simulation.addCharge sign position model.activeSimulation }
                    else
                        model
                _ ->
                    model
            , Cmd.none
            )

        CursorClicked position ->
            ( handleCursorClick position model
            , Cmd.none 
            )

        MouseDown ->
            ( { model | isMouseDown = True }, Cmd.none )

        MouseUp ->
            ( { model | isMouseDown = False }, Cmd.none )

handleCursorClick : Position -> Model -> Model
handleCursorClick position model =
    case model.cursor of
        Painter sign ->
            { model | activeSimulation = Simulation.addCharge sign position model.activeSimulation }
            
        Deleter ->
            let
                dist p1 p2 = Vector2.distance (vec2 (Tuple.first p1) (Tuple.second p1)) p2
                
                findClickedCharge =
                    List.foldl
                        (\field maybeId ->
                            case maybeId of
                                Just id -> Just id
                                Nothing ->
                                    if dist position field.source.position <= field.source.r then
                                        Just field.source.id
                                    else
                                        Nothing
                        )
                        Nothing
                        model.activeSimulation.fields
            in
            case findClickedCharge of
                Just id ->
                    { model | activeSimulation = Simulation.deleteCharge id model.activeSimulation }
                Nothing ->
                    model
                    
        _ ->
            model

updateSimulationSize : Float -> Float -> Model -> Model
updateSimulationSize width height model =
    { model 
        | activeSimulation = 
            let
                newHeight = height - 150
            in
            if model.activeSimulation.width /= width || model.activeSimulation.height /= newHeight then
                Simulation.init width newHeight
            else
                model.activeSimulation
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SimulationMsg (Simulation.subscriptions model.activeSimulation)
        , onResize WindowResized
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
