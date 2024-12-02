module Main exposing (main)

import Browser
import Browser.Events
import Element as E
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import List.Extra
import Math.Vector2 as Vector2
import Simulation
import Task
import Utils exposing (toElmUiColor)
import Browser.Dom

type alias Model =
    { simulations : List Simulation.Model
    , activeSimulation : Simulation.Model
    , defaultSimulationIndex : Int
    , simulationWidth : Float
    , simulationHeight : Float
    , cursor : Cursor
    , isMouseDown : Bool
    , isInteractionEnabled : Bool
    , isMobile : Bool
    }


type Cursor
    = Selector
    | Painter Simulation.Sign
    | Deleter


type Msg
    = ChangeActiveSimulation Simulation.Model
    | SimulationMsg Simulation.Msg
    | GotViewport Browser.Dom.Viewport
    | WindowResized Int Int
    | UpdateActiveSimulationState
    | UpdateCursor Cursor
    | DrawCharges Simulation.Position
    | CursorClicked Simulation.Position
    | MouseDown
    | MouseUp


defaultSimulationWidth : Float
defaultSimulationWidth =
    1200


defaultSimulationHeight : Float
defaultSimulationHeight =
    750


init : ( Model, Cmd Msg )
init =
    let
        defaultActiveSimulation =
            let
                simulation =
                    Simulation.init defaultSimulationWidth defaultSimulationHeight
            in
                simulation

        defaultSimulations =
            [ defaultActiveSimulation ]

        project =
            { simulations = defaultSimulations
            , activeSimulation = defaultActiveSimulation
            , defaultSimulationIndex = 1
            , simulationWidth = defaultSimulationWidth
            , simulationHeight = defaultSimulationHeight
            , cursor = Selector
            , isMouseDown = False
            , isInteractionEnabled = True
            , isMobile = False
            }
    in
    ( project
    , Task.perform GotViewport Browser.Dom.getViewport
    )


view : Model -> Html Msg
view model =
    E.layout
        [ E.width E.fill
        , E.height E.fill
        , Font.size 14
        , Font.family [ Font.monospace ]
        , Background.color <| toElmUiColor model.activeSimulation.settings.colors.background
        , E.htmlAttribute <| Html.Attributes.style "touch-action" "none"
        ]
    <|
        E.el
            (E.centerX
                :: (if model.isMobile then
                        []
                    else
                        []
                   )
            )
            (E.el
                (if model.isInteractionEnabled then
                    [ E.htmlAttribute <| Mouse.onMove (\event -> DrawCharges event.offsetPos)
                    , E.htmlAttribute <| Mouse.onClick (\event -> CursorClicked event.offsetPos)
                    , E.htmlAttribute <| Mouse.onDown (\_ -> MouseDown)
                    , E.htmlAttribute <| Mouse.onUp (\_ -> MouseUp)
                    ]
                 else
                    []
                )
                (E.html (Html.map SimulationMsg <| Simulation.view model.activeSimulation))
            )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of

        ChangeActiveSimulation newSimulation ->
            ( changeActiveSimulation newSimulation model, Cmd.none )

        SimulationMsg msg ->
            updateActiveSimulationWithMsg msg model

        GotViewport viewport ->
            ( updateSimulationSize viewport.viewport.width viewport.viewport.height model, Cmd.none )

        WindowResized newWidth newHeight ->
            ( updateSimulationSize (toFloat newWidth) (toFloat newHeight) model, Cmd.none )

        UpdateActiveSimulationState ->
            ( updateActiveSimulationState model, Cmd.none )

        UpdateCursor newCursor ->
            ( updateCursor newCursor model, Cmd.none )

        DrawCharges position ->
            ( drawCharges position model, Cmd.none )

        CursorClicked position ->
            ( cursorClicked position model, Cmd.none )

        MouseDown ->
            ( mouseDown model, Cmd.none )

        MouseUp ->
            ( mouseUp model, Cmd.none )

changeActiveSimulation : Simulation.Model -> Model -> Model
changeActiveSimulation newSimulation model =
    { model
        | activeSimulation =
            newSimulation
    }


updateActiveSimulationWithMsg : Simulation.Msg -> Model -> ( Model, Cmd Msg )
updateActiveSimulationWithMsg msg model =
    let
        ( newSimulation, cmd ) =
            Simulation.update msg model.activeSimulation
    in
    ( updateActiveSimulation newSimulation model
    , Cmd.map SimulationMsg cmd
    )


updateActiveSimulation : Simulation.Model -> Model -> Model
updateActiveSimulation newActiveSimulation model =
    { model
        | activeSimulation =
            newActiveSimulation
        , simulations =
            List.map
                (\simulation ->
                    if simulation == model.activeSimulation then
                        newActiveSimulation

                    else
                        simulation
                )
                model.simulations
    }

updateIsInteractionEnabled : Bool -> Model -> Model
updateIsInteractionEnabled isInteractionEnabled model =
    let
        oldSimulation =
            model.activeSimulation

        newSimulation =
            { oldSimulation
                | isInteractionEnabled =
                    isInteractionEnabled
            }
    in
    updateActiveSimulation
        newSimulation
        { model | isInteractionEnabled = isInteractionEnabled }


updateSimulationSize : Float -> Float -> Model -> Model
updateSimulationSize newWidth newHeight model =
    let
        isMobile =
            min newWidth newHeight < 500

        newHeightAdapted =
            if isMobile then
                newHeight - 60

            else
                newHeight - 150

        updateSize =
            \simulation ->
                if simulation.width /= newWidth || simulation.height /= newHeightAdapted then
                    Simulation.init newWidth newHeightAdapted

                else
                    simulation
    in
    { model
        | simulationWidth =
            newWidth
        , simulationHeight =
            newHeightAdapted
        , simulations =
            List.map
                updateSize
                model.simulations
        , activeSimulation =
            updateSize model.activeSimulation
        , isMobile =
            isMobile
    }


updateActiveSimulationState : Model -> Model
updateActiveSimulationState model =
    let
        activeSimulation =
            model.activeSimulation

        nextState =
            case activeSimulation.state of
                Simulation.Running ->
                    Simulation.Resting

                Simulation.Resting ->
                    Simulation.Running
    in
    { model
        | activeSimulation =
            { activeSimulation
                | state =
                    nextState
            }
    }


updateCursor : Cursor -> Model -> Model
updateCursor newCursor model =
    let
        oldIsInteractionEnabled =
            model.isInteractionEnabled

        newModel =
            updateIsInteractionEnabled (newCursor /= Deleter)
                { model
                    | cursor = newCursor
                }
    in
    { newModel
        | isInteractionEnabled = oldIsInteractionEnabled
    }

drawCharges : Simulation.Position -> Model -> Model
drawCharges position model =
    case model.cursor of
        Painter sign ->
            if model.isMouseDown then
                updateActiveSimulation
                    (Simulation.addCharge sign position model.activeSimulation)
                    model

            else
                model

        _ ->
            model


cursorClicked : Simulation.Position -> Model -> Model
cursorClicked position model =
    case model.cursor of
        Painter sign ->
            updateActiveSimulation
                (Simulation.addCharge sign position model.activeSimulation)
                model

        Deleter ->
            let
                clickedChargedId =
                    List.Extra.findMap
                        (\field ->
                            if
                                Vector2.distance
                                    (Vector2.vec2 (Tuple.first position) (Tuple.second position))
                                    field.source.position
                                    <= field.source.r
                            then
                                Just field.source.id

                            else
                                Nothing
                        )
                        model.activeSimulation.fields
            in
            case clickedChargedId of
                Just id ->
                    updateActiveSimulation
                        (Simulation.deleteCharge id model.activeSimulation)
                        model

                Nothing ->
                    model

        _ ->
            model


mouseDown : Model -> Model
mouseDown model =
    { model
        | isMouseDown =
            True
    }


mouseUp : Model -> Model
mouseUp model =
    { model
        | isMouseDown =
            False
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SimulationMsg <| Simulation.subscriptions model.activeSimulation
        , Browser.Events.onResize WindowResized
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
