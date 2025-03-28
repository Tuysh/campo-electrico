module Simulation exposing (Model, Msg, Position, Settings, Sign(..), addCharge, calculateFields, defaultSettings, deleteCharge, init, subscriptions, update, view)

import Color exposing (Color)
import Dict exposing (Dict)
import Dict.Extra
import Draggable
import Draggable.Events
import Element as E
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import List.Extra
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Maybe.Extra
import Set exposing (Set)
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (Cursor(..), Paint(..), Transform(..), px)
import Utils exposing (styles)
import Color exposing (rgba)
import Json.Decode as Decode

type alias Model =
    { fields : List Field
    , activeSourceId : Maybe Id
    , nextId : Id
    , drag : Draggable.State Id
    , contextMenu : ContextMenu
    , settings : Settings
    , width : Float
    , height : Float
    , isInteractionEnabled : Bool
    }


type alias Settings =
    { r : Float
    , density : Int
    , steps : Int
    , delta : Float
    , magnitude : Float
    , colors : SettingColors
    }


type alias SettingColors =
    { positiveCharge : Color
    , negativeCharge : Color
    , positiveLine : Color
    , negativeLine : Color
    , background : Color
    }


type ContextMenu
    = FieldContextMenu
    | GeneralContextMenu Position
    | NoContextMenu


type alias Field =
    { source : Charge
    , density : Int
    , steps : Int
    , delta : Float
    , lines : List Line
    }


type alias Id =
    Int


type alias Line =
    ( Id, List Point, Maybe Id )


type alias Point =
    ( Float, Float )


type alias Position =
    Point


type Sign
    = Positive
    | Negative


type alias Charge =
    { id : Id
    , sign : Sign
    , magnitude : Float
    , position : Vec2
    , velocity : Vec2
    , r : Float
    }

defaultSettings : Settings
defaultSettings =
    { r = 20.0
    , density = 10
    , steps = 3000
    , delta = 2
    , magnitude = 1
    , colors =
        { positiveCharge = rgba (193 / 255) (18 / 255) (31 / 255) 1
        , negativeCharge = rgba (102 / 255) (155 / 255) (188 / 255) 1
        , positiveLine = rgba 255 255 255 1
        , negativeLine = rgba 255 255 255 1
        , background = Color.black
        }
    }
    
type Msg
    = OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg Id)
    | StartDragging Id
    | EndDragging
    | ActivateSource Id
    | ToggleSourceSign
    | UpdateSourceCharge Sign
    | ShowFieldContextMenu
    | ShowGeneralContextMenu Mouse.Event
    | DeleteActiveField
    | ClickedBackground
    | DuplicateActiveField
    | DeselectActiveField
    | AddPositiveCharge Position
    | AddNegativeCharge Position


init : Float -> Float -> Model
init width height =
    let
        halfWidth =
            width / 2

        defaultFields =
            [ { source = { id = 0, sign = Negative, magnitude = 1.0, position = vec2 halfWidth (height * 1 / 3), velocity = vec2 0 0, r = defaultSettings.r }
              , density = defaultSettings.density
              , steps = defaultSettings.steps
              , delta = defaultSettings.delta
              , lines = []
              }
            , { source = { id = 1, sign = Positive, magnitude = 1.0, position = vec2 halfWidth (height * 2 / 3), velocity = vec2 0 0, r = defaultSettings.r }
              , density = defaultSettings.density
              , steps = defaultSettings.steps
              , delta = defaultSettings.delta
              , lines = []
              }
            ]

        defaultModel =
            { fields =
                calculateFields width height defaultFields
            , activeSourceId =
                if List.length defaultFields > 0 then
                    Just 0

                else
                    Nothing
            , nextId = List.length defaultFields
            , drag = Draggable.init
            , contextMenu = NoContextMenu
            , settings = defaultSettings
            , width = width
            , height = height
            , isInteractionEnabled = True
            }
    in
    defaultModel


countFieldLinesEndingWithChargeId : Id -> Field -> Int
countFieldLinesEndingWithChargeId id field =
    List.sum <|
        List.map
            (\( _, _, endChargeId ) ->
                if endChargeId == Just id then
                    1

                else
                    0
            )
            field.lines


leanifyFields : List Field -> List Field
leanifyFields fields =
    let
        sourceToDestination =
            findDuplicateFieldLines fields
    in
    List.map
        (\field ->
            case Dict.get field.source.id sourceToDestination of
                Just destinationIds ->
                    { field
                        | lines =
                            List.filter
                                (\( _, _, endChargeId ) ->
                                    case endChargeId of
                                        Just id ->
                                            not (Set.member id destinationIds)

                                        Nothing ->
                                            True
                                )
                                field.lines
                    }

                Nothing ->
                    field
        )
        fields


findDuplicateFieldLines : List Field -> Dict Id (Set Id)
findDuplicateFieldLines fields =
    Dict.Extra.fromListDedupe Set.union <|
        List.map
            (\( fieldA, fieldB ) ->
                let
                    chargeA =
                        fieldA.source.id

                    chargeB =
                        fieldB.source.id

                    numOfLinesFromAtoB =
                        countFieldLinesEndingWithChargeId chargeB fieldA

                    numOfLinesFromBtoA =
                        countFieldLinesEndingWithChargeId chargeA fieldB
                in
                if numOfLinesFromAtoB > numOfLinesFromBtoA then
                    ( chargeB, Set.singleton chargeA )

                else
                    ( chargeA, Set.singleton chargeB )
            )
            (List.Extra.uniquePairs fields)


calculateFields : Float -> Float -> List Field -> List Field
calculateFields width height fields =
    leanifyFields <|
        List.map
            (\field ->
                let
                    deltaAngle =
                        2 * pi / toFloat field.density

                    lines =
                        List.map
                            (\index ->
                                let
                                    angle =
                                        deltaAngle * index

                                    dx =
                                        field.source.r * cos angle

                                    dy =
                                        field.source.r * sin angle

                                    start =
                                        Vector2.add (vec2 dx dy) field.source.position
                                in
                                calculateFieldLine
                                    { charges = List.map .source fields
                                    , steps = field.steps
                                    , delta = field.delta
                                    , sourceSign = field.source.sign
                                    , startChargeId = field.source.id
                                    , start = ( Vector2.getX start, Vector2.getY start )
                                    , xBound = width * 1.5
                                    , yBound = height * 1.5
                                    }
                            )
                            (List.map toFloat <| List.range 0 (field.density - 1))
                in
                { field
                    | lines = lines
                }
            )
            fields


calculateFieldLine :
    { charges : List Charge
    , steps : Int
    , delta : Float
    , sourceSign : Sign
    , startChargeId : Id
    , start : Point
    , xBound : Float
    , yBound : Float
    }
    -> Line
calculateFieldLine { charges, steps, delta, sourceSign, startChargeId, start, xBound, yBound } =
    foldlWhile
        (\_ line ->
            let
                ( _, points, _ ) =
                    line

                ( x, y ) =
                    case points of
                        prev :: _ ->
                            prev

                        _ ->
                            ( 0, 0 )

                -- impossible
                previousPosition =
                    vec2 x y

                outOfBounds =
                    x > xBound || x < 0 || y > yBound || y < 0

                reachedAChargeWithId =
                    List.Extra.findMap
                        (\charge ->
                            if
                                charge.id
                                    /= startChargeId
                                    && Vector2.distance charge.position previousPosition
                                    <= 5
                            then
                                Just charge.id

                            else
                                Nothing
                        )
                        charges

                stopCalculation =
                    outOfBounds || Maybe.Extra.isJust reachedAChargeWithId

                netField =
                    if stopCalculation then
                        Vector2.vec2 0 0

                    else
                        List.foldl
                            (\charge sum ->
                                let
                                    d =
                                        Vector2.distance previousPosition charge.position / 100

                                    magnitude =
                                        charge.magnitude / (d ^ 2)

                                    sign =
                                        case charge.sign of
                                            Positive ->
                                                1

                                            Negative ->
                                                -1

                                    field =
                                        Vector2.scale (sign * magnitude) <|
                                            Vector2.normalize <|
                                                Vector2.sub previousPosition charge.position
                                in
                                Vector2.add sum field
                            )
                            (Vector2.vec2 0 0)
                            charges

                next =
                    if stopCalculation then
                        ( x, y )

                    else
                        let
                            vec =
                                Vector2.add
                                    (Vector2.vec2 x y)
                                    ((case sourceSign of
                                        Positive ->
                                            identity

                                        Negative ->
                                            Vector2.negate
                                     )
                                     <|
                                        Vector2.scale delta <|
                                            Vector2.normalize netField
                                    )
                        in
                        ( Vector2.getX vec, Vector2.getY vec )
            in
            ( ( startChargeId, next :: points, reachedAChargeWithId ), stopCalculation )
        )
        ( startChargeId, [ start ], Nothing )
        (List.range 0 (steps - 1))


dragConfig : Bool -> Draggable.Config Id Msg
dragConfig isInteractionEnabled =
    Draggable.customConfig
        (if isInteractionEnabled then
            [ Draggable.Events.onDragBy OnDragBy
            , Draggable.Events.onDragStart StartDragging
            , Draggable.Events.onDragEnd EndDragging
            , Draggable.Events.onClick ActivateSource
            ]

         else
            []
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnDragBy offsetPos ->
            ( onDragBy offsetPos model, Cmd.none )

        StartDragging id ->
            ( startDragging id model, Cmd.none )

        EndDragging ->
            ( endDragging model, Cmd.none )

        ActivateSource id ->
            ( setActiveSourceId id model, Cmd.none )

        ToggleSourceSign ->
            ( toggleSourceSign model, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update (dragConfig model.isInteractionEnabled) dragMsg model

        ShowFieldContextMenu ->
            ( showFieldContextMenu model, Cmd.none )

        ShowGeneralContextMenu { offsetPos } ->
            ( showGeneralContextMenu offsetPos model, Cmd.none )

        DeleteActiveField ->
            ( deleteActiveField model, Cmd.none )

        ClickedBackground ->
            ( resetState model, Cmd.none )

        DuplicateActiveField ->
            ( duplicateActiveField model, Cmd.none )

        DeselectActiveField ->
            ( deselectActiveField model, Cmd.none )

        AddPositiveCharge position ->
            ( addCharge Positive position model, Cmd.none )
            
        AddNegativeCharge position ->
            ( addCharge Negative position model, Cmd.none )
            
        UpdateSourceCharge sign ->
            ( updateSourceCharge sign model, Cmd.none )

updateSourceCharge : Sign -> Model -> Model
updateSourceCharge sign model =
    let
        newFields =
            updateActive
                (\field ->
                    let
                        source =
                            field.source

                        delta =
                            case sign of
                                Positive ->
                                    1.0

                                Negative ->
                                    -1.0

                        newMagnitude =
                            source.magnitude
                                + (case source.sign of
                                    Positive ->
                                        delta

                                    Negative ->
                                        -delta
                                  )
                    in
                    { field
                        | source =
                            { source
                                | magnitude = min 20 <| max 1.0 <| newMagnitude
                                , sign =
                                    if abs newMagnitude < 1.0 then
                                        flipSign source.sign

                                    else
                                        source.sign
                            }
                    }
                )
                model.activeSourceId
                model.fields
    in
    { model
        | fields =
            calculateFields model.width model.height newFields
    }

onDragBy : Position -> Model -> Model
onDragBy offsetPos model =
    let
        newFields =
            updateActive (dragSource offsetPos) model.activeSourceId model.fields
    in
    { model
        | fields =
            calculateFields model.width model.height newFields
    }


startDragging : Id -> Model -> Model
startDragging id model =
    setActiveSourceId id <| optimizeModel model


optimizeModel : Model -> Model
optimizeModel model =
    { model
        | fields =
            List.map
                (\field ->
                    if field.delta <= 7 then
                        { field
                            | delta =
                                field.delta * 3
                            , steps =
                                round <| toFloat field.steps / 3
                        }

                    else
                        field
                )
                model.fields
    }


endDragging : Model -> Model
endDragging model =
    deoptimizeModel model


deoptimizeModel : Model -> Model
deoptimizeModel model =
    { model
        | fields =
            calculateFields model.width model.height <|
                List.map
                    (\field ->
                        if field.delta <= 7 then
                            { field
                                | delta =
                                    field.delta / 3
                                , steps =
                                    field.steps * 3
                            }

                        else
                            field
                    )
                    model.fields
    }

setActiveSourceId : Id -> Model -> Model
setActiveSourceId id model =
    { model
        | activeSourceId = Just id
    }


toggleSourceSign : Model -> Model
toggleSourceSign model =
    let
        newFields =
            updateActive
                (\field ->
                    let
                        source =
                            field.source
                    in
                    { field
                        | source =
                            { source
                                | sign =
                                    negateSign field.source.sign
                            }
                    }
                )
                model.activeSourceId
                model.fields
    in
    { model
        | fields =
            calculateFields model.width model.height newFields
    }

showFieldContextMenu : Model -> Model
showFieldContextMenu model =
    { model
        | contextMenu =
            case model.activeSourceId of
                Nothing ->
                    model.contextMenu

                Just _ ->
                    FieldContextMenu
    }


showGeneralContextMenu : Position -> Model -> Model
showGeneralContextMenu offsetPos model =
    { model
        | contextMenu =
            GeneralContextMenu offsetPos
    }


deleteActiveField : Model -> Model
deleteActiveField model =
    let
        newFields =
            case model.activeSourceId of
                Nothing ->
                    model.fields

                Just id ->
                    List.filter
                        (\field ->
                            field.source.id /= id
                        )
                        model.fields
    in
    { model
        | fields =
            calculateFields model.width model.height newFields
        , contextMenu =
            NoContextMenu
        , activeSourceId =
            Nothing
    }


deleteCharge : Id -> Model -> Model
deleteCharge id model =
    let
        newFields =
            List.filter
                (\field ->
                    field.source.id /= id
                )
                model.fields
    in
    { model
        | fields =
            calculateFields model.width model.height newFields
        , contextMenu =
            NoContextMenu
        , activeSourceId =
            if Just id == model.activeSourceId then
                Nothing

            else
                model.activeSourceId
    }


addCharge : Sign -> Position -> Model -> Model
addCharge sign ( x, y ) model =
    let
        newCharge : Charge
        newCharge =
            { sign = sign
            , magnitude = model.settings.magnitude
            , position = vec2 x y
            , velocity = vec2 0 0
            , r = model.settings.r
            , id = model.nextId
            }

        newField : Field
        newField =
            { source = newCharge
            , density = model.settings.density
            , steps = model.settings.steps
            , delta = model.settings.delta
            , lines = []
            }

        newFields : List Field
        newFields =
            newField :: model.fields
    in
    { model
        | fields =
            calculateFields model.width model.height newFields
        , nextId =
            model.nextId + 1
    }


duplicateActiveField : Model -> Model
duplicateActiveField model =
    let
        duplicatedFields =
            List.indexedMap
                (\index field ->
                    let
                        source =
                            field.source
                    in
                    { field
                        | source =
                            { source
                                | position =
                                    Vector2.add (vec2 (source.r * 2 + 15) 0) source.position
                                , id =
                                    model.nextId + index
                            }
                    }
                )
                (getActiveFields model)

        newFields =
            model.fields ++ duplicatedFields
    in
    { model
        | fields =
            calculateFields model.width model.height newFields
        , nextId =
            model.nextId + List.length duplicatedFields
    }


deselectActiveField : Model -> Model
deselectActiveField model =
    { model
        | activeSourceId =
            Nothing
    }


resetState : Model -> Model
resetState model =
    { model
        | contextMenu =
            NoContextMenu
    }


updateActive : (Field -> Field) -> Maybe Id -> List Field -> List Field
updateActive func activeId fields =
    case activeId of
        Nothing ->
            fields

        Just id ->
            List.map
                (\field ->
                    if field.source.id == id then
                        func field

                    else
                        field
                )
                fields


dragSource : Position -> Field -> Field
dragSource ( dx, dy ) field =
    let
        source =
            field.source
    in
    { field
        | source =
            { source
                | position =
                    Vector2.add (Vector2.vec2 dx dy) source.position
            }
    }


view : Model -> Html Msg
view model =
    E.layout
        ([ E.width E.fill
         , E.height E.fill
         , Font.size 16
         , Font.family
            [ Font.monospace
            ]
         ]
            ++ (if model.isInteractionEnabled then
                    [ Element.Events.onClick ClickedBackground ]

                else
                    []
               )
        )
    <|
        E.el
            [ E.inFront <| viewContextMenu model
            , E.centerX
            , E.centerY
            ]
            (E.html <|
                Svg.svg
                    [ Attributes.width (px model.width)
                    , Attributes.height (px model.height)
                    , Attributes.viewBox 0 0 model.width model.height
                    , Attributes.id "modelSvg"
                    , Mouse.onContextMenu ShowGeneralContextMenu
                    ]
                <|
                    viewBackground model.width model.height model.settings.colors.background
                        :: List.map (viewFieldLines model.settings) model.fields
                        ++ List.map (viewFieldSource model.isInteractionEnabled model.activeSourceId model.settings) model.fields
            )


viewBackground : Float -> Float -> Color -> Svg Msg
viewBackground width height color =
    Svg.rect
        [ Attributes.width <| px width
        , Attributes.height <| px height
        , Attributes.fill <| Paint color
        ]
        []


viewContextMenu : Model -> E.Element Msg
viewContextMenu model =
    case model.contextMenu of
        FieldContextMenu ->
            viewFieldContextMenu styles.button model

        GeneralContextMenu position ->
            viewGeneralContextMenu styles.button position

        NoContextMenu ->
            -- very weired. Should be `E.none` but need below for buttons to be styled correctly
            E.el [ E.htmlAttribute <| Html.Attributes.style "display" "none" ] <|
                viewFieldContextMenu styles.button model


getActiveFields : Model -> List Field
getActiveFields model =
    case model.activeSourceId of
        Just id ->
            List.filter
                (\field ->
                    field.source.id == id
                )
                model.fields

        Nothing ->
            []


viewFieldContextMenu : List (E.Attribute Msg) -> Model -> E.Element Msg
viewFieldContextMenu menuItemstyless model =
    let
        ( x, y ) =
            case List.head <| getActiveFields model of
                Just field ->
                    ( Vector2.getX field.source.position, Vector2.getY field.source.position )

                Nothing ->
                    ( 0, 0 )

        -- impossible
    in
    E.column
        [ E.moveRight x
        , E.moveDown y
        ]
        [ Input.button
            menuItemstyless
            { onPress = Just DeleteActiveField
            , label = E.text "Eliminar"
            }
        , Input.button
            menuItemstyless
            { onPress = Just DuplicateActiveField
            , label = E.text "Duplicar"
            }
        , Input.button
            menuItemstyless
            { onPress = Just DeselectActiveField
            , label = E.text "Deseleccionar"
            }
        ]


viewGeneralContextMenu : List (E.Attribute Msg) -> Position -> E.Element Msg
viewGeneralContextMenu menuItemstyless ( x, y ) =
    E.column
        [ E.moveRight x
        , E.moveDown y
        ]
        [ Input.button
            menuItemstyless
            { onPress = Just <| AddPositiveCharge ( x, y )
            , label = E.text "Añadir + carga"
            }
        , Input.button
            menuItemstyless
            { onPress = Just <| AddNegativeCharge ( x, y )
            , label = E.text "Añadir - carga"
            }
        ]


viewFieldSource : Bool -> Maybe Id -> Settings -> Field -> Svg Msg
viewFieldSource isInteractionEnabled activeSourceId settings field =
    let
        fill =
            case field.source.sign of
                Positive ->
                    settings.colors.positiveCharge

                Negative ->
                    settings.colors.negativeCharge

        gradientId =
            "gradient" ++ String.fromInt field.source.id

        x =
            Vector2.getX field.source.position

        y =
            Vector2.getY field.source.position
    in
    Svg.g []
        [ Svg.defs []
            [ Svg.radialGradient
                [ Attributes.id <| gradientId ]
                [ Svg.stop
                    [ Attributes.offset "1%"
                    , Attributes.stopColor <| Color.toCssString <| setAlpha 1 fill
                    ]
                    []
                , Svg.stop
                    [ Attributes.offset "100%"
                    , Attributes.stopColor <| Color.toCssString <| setAlpha 0.2 fill
                    ]
                    []
                ]
            ]
        , Svg.circle
            [ Attributes.cx (px x)
            , Attributes.cy (px y)
            , Attributes.r (px <| lerp 0 20 10 40 (min 20 field.source.r * field.source.magnitude / 10))
            , Attributes.fill <| Reference gradientId
            , Html.Attributes.style "pointer-events" "none"
            ]
            []
        , Svg.circle
            (([ Attributes.cx (px x)
              , Attributes.cy (px y)
              , Attributes.r (px field.source.r)
              , Attributes.fill <| Paint fill
              , Html.Events.onDoubleClick ToggleSourceSign
              , onRightClick ShowFieldContextMenu  -- Añadido aquí para todas las cargas
              ]
                ++ (if Just field.source.id == activeSourceId then
                        [ ]

                    else
                        []
                   )
                ++ (if isInteractionEnabled then
                        Draggable.mouseTrigger field.source.id DragMsg
                            :: Draggable.touchTriggers field.source.id DragMsg

                    else
                        []
                   )
             )
                ++ (case activeSourceId of
                        Just id ->
                            if field.source.id == id then
                                [ Attributes.id "activeSource"
                                , Attributes.stroke <| Paint Color.lightGreen
                                , Attributes.strokeWidth <| px 8
                                ]

                            else
                                []

                        Nothing ->
                            []
                   )
            )
            [ Svg.animate
                [ Attributes.attributeName "stroke-opacity"
                , Attributes.animationValues [ 1, 0.3, 1 ]
                , Attributes.dur <| TypedSvg.Types.Duration "3s"
                , Attributes.repeatCount TypedSvg.Types.RepeatIndefinite
                ]
                []
            ]
        , case activeSourceId of
            Just id ->
                let
                    tooltipWidth =
                        160

                    tooltipHeight =
                        40
                in
                if field.source.id == id  then
                    Svg.g
                        [ Attributes.transform
                            [ Translate (x - tooltipWidth / 2) (y - field.source.r - tooltipHeight - 10)
                            ]
                        , Attributes.id "sourceValueControl"
                        ]
                        [ Svg.rect
                            [ Attributes.x <| px 0
                            , Attributes.y <| px -10
                            , Attributes.width <| px tooltipWidth
                            , Attributes.height <| px tooltipHeight
                            , Attributes.fill <| Paint Color.lightGrey
                            , Attributes.rx <| px 15
                            , Attributes.ry <| px 15
                            ]
                            []
                        , Svg.g
                            [ Attributes.cursor CursorPointer
                            , TypedSvg.Events.onClick (UpdateSourceCharge Negative)
                            ]
                            [ Svg.rect
                                [ Attributes.x <| (px <| 0)
                                , Attributes.y <| (px <| -10)
                                , Attributes.width <| px 40
                                , Attributes.height <| px 40
                                , Attributes.fill <| Paint settings.colors.negativeCharge
                                , Attributes.rx <| px 15
                                , Attributes.ry <| px 15
                                ]
                                []
                            , Svg.text_
                                [ Attributes.x (px <| 12)
                                , Attributes.y (px <| 20)
                                , Attributes.stroke <| Paint settings.colors.background
                                , Attributes.fill <| Paint settings.colors.background
                                , Attributes.fontSize (px <| 26)
                                ]
                                [ TypedSvg.Core.text "<"
                                ]
                            ]
                        , Svg.text_
                            [ Attributes.x
                                (px <|
                                    if abs field.source.magnitude < 10 then
                                        50
                                    else
                                        45
                                )
                            , Attributes.y (px <| 20)
                            , Attributes.stroke <| Paint ( rgba (33 / 255) (33 / 255) (33 / 255) 1)
                            , Attributes.fontSize (px <| 24)
                            , Attributes.cursor CursorDefault
                            ]
                            [ TypedSvg.Core.text (signToString field.source.sign ++ String.fromInt (round field.source.magnitude))
                            ]
                        , Svg.g
                            [ Attributes.cursor CursorPointer
                            , TypedSvg.Events.onClick (UpdateSourceCharge Positive)
                            ]
                            [ Svg.rect
                                [ Attributes.x <| (px <| tooltipWidth - 40)
                                , Attributes.y <| (px <| -10)
                                , Attributes.width <| px 40
                                , Attributes.height <| px 40
                                , Attributes.fill <| Paint settings.colors.positiveCharge
                                , Attributes.rx <| px 15
                                , Attributes.ry <| px 15
                                ]
                                []
                            , Svg.text_
                                [ Attributes.x (px <| tooltipWidth - 27)
                                , Attributes.y (px <| 20)
                                , Attributes.stroke <| Paint settings.colors.background
                                , Attributes.fill <| Paint settings.colors.background
                                , Attributes.cursor CursorPointer
                                , Attributes.fontSize (px <| 26)
                                ]
                                [ TypedSvg.Core.text ">"
                                ]
                            ]
                        ]

                else
                    Svg.g [] []

            Nothing ->
                Svg.g [] []
        ]


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    Html.Events.custom "contextmenu"
        (Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )

viewFieldLines : Settings -> Field -> Svg Msg
viewFieldLines settings field =
    let
        lineColor =
            case field.source.sign of
                Positive ->
                    settings.colors.positiveLine

                Negative ->
                    settings.colors.negativeLine
    in
    Svg.g [] <|
        List.map
            (\( _, line, _ ) ->
                Svg.polyline
                    [ Attributes.fill PaintNone, Attributes.stroke <| Paint lineColor, Attributes.points line ]
                    []
            )
            field.lines


signToString : Sign -> String
signToString sign =
    case sign of
        Positive ->
            "+"

        Negative ->
            "-"


flipSign : Sign -> Sign
flipSign sign =
    case sign of
        Positive ->
            Negative

        Negative ->
            Positive


setAlpha : Float -> Color -> Color
setAlpha alpha color =
    let
        rgba =
            Color.toRgba color
    in
    Color.fromRgba <|
        { rgba
            | alpha = alpha
        }


negateSign : Sign -> Sign
negateSign sign =
    case sign of
        Positive ->
            Negative

        Negative ->
            Positive


lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp min1 max1 min2 max2 num =
    let
        ratio =
            abs <| (num - min1) / (max1 - min1)
    in
    min2 + ratio * (max2 - min2)

foldlWhile : (a -> b -> ( b, Bool )) -> b -> List a -> b
foldlWhile accumulate initial list =
    let
        foldlHelper accumulated aList =
            case aList of
                head :: tail ->
                    let
                        ( nextAccumulated, break ) =
                            accumulate head accumulated
                    in
                    if break then
                        nextAccumulated

                    else
                        foldlHelper nextAccumulated tail

                [] ->
                    accumulated
    in
    foldlHelper initial list


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag
