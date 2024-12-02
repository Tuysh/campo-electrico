module Utils exposing (toElmUiColor, styles)

import Color
import Element as E
import Element.Background as Background
import Element.Border as Border

toElmUiColor : Color.Color -> E.Color
toElmUiColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha

styles : { button : List (E.Attr () msg) }
styles =
    { button =
        [ Background.color (E.rgb 0.87 0.87 0.87)  -- lightGrey
        , E.mouseOver
            [ Background.color (E.rgb 0.97 0.97 0.97) ]  -- white
        , E.paddingXY 10 5
        , E.width <| E.px 150
        , Border.widthXY 2 1
        , Border.color (E.rgb 0.33 0.33 0.33)  -- darkGrey
        ]
    }
