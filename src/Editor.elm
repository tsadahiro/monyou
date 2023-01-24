module Editor exposing (..)

import Html.Events.Extra.Pointer exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Element exposing (..)
import Svg exposing (svg, rect)
import Svg.Attributes as SAt
import Asanoha
import Ichimatsu
import Rokkaku
import P3
import PrimitivesView exposing (..)
import Types exposing (..)


editor : Model -> Element Msg
editor model =
    let
        monyou = model.monyou
        relativePos : Event -> {x:Float, y:Float}
        relativePos event =
            { x = ((Tuple.first event.pointer.offsetPos) - (4*10))/4
                  --x = ((Tuple.first event.pointer.offsetPos) - (toFloat (4*model.editorMargin)))/4
            ,y = ((Tuple.second event.pointer.offsetPos) - (toFloat (4*model.editorMargin)))/4
            }
        mousePos : Mouse.Event -> {x:Float, y:Float}
        mousePos event =
            { --x = ((Tuple.first event.offsetPos) - (toFloat (4*model.editorMargin)))/4
              x = ((Tuple.first event.offsetPos) - (4*10))/4
            ,y = ((Tuple.second event.offsetPos) - (toFloat (4*model.editorMargin)))/4
            }
    in
        html (svg [ SAt.width "600"
                  , SAt.height "600"
                  , onDown (relativePos >> Down)
                  , onUp (relativePos >> Up)
                  , onMove (relativePos >> Move)
                  , Mouse.onClick (mousePos >> Click)
                  , Mouse.onDoubleClick (\ev -> DoubleClick)
                  ]
                  [Svg.g[SAt.transform ("scale(4,4)translate("
                                            --++ (String.fromInt model.editorMargin)
                                            ++ "10"
                                            ++ "," ++ (String.fromInt model.editorMargin) ++ ")")]
                       ([Svg.clipPath [SAt.id ("cell0")]
                             [case monyou of
                                  Asanoha -> Asanoha.fundPath ((toFloat model.unit))
                                  Ichimatsu -> Ichimatsu.fundPath ((toFloat model.unit))
                                  Rokkaku -> Rokkaku.fundPath ((toFloat model.unit))
                                  P3 -> P3.fundPath ((toFloat model.unit))
                             ]
                        ,rect [SAt.width "150"
                              ,SAt.height "150"
                              --,SAt.x (String.fromInt -model.editorMargin)
                              ,SAt.x (String.fromInt -10)
                              ,SAt.y (String.fromInt -model.editorMargin)
                              ,SAt.fill "#eee"
                              ,SAt.stroke "black"
                              ][]
                        ,rect [SAt.width "100%"
                              ,SAt.height "100%"
                              ,SAt.x (String.fromInt -10)
                              ,SAt.y (String.fromInt -model.editorMargin)
                              ,SAt.fill "white"
                              ,SAt.clipPath ("url(#cell0)")
                              ][]
                        ]++ (List.map (\shape -> shapeView shape "0" (2*model.unit)) model.shapes)
                            ++ (case model.newShape of
                                    Just shape ->
                                        [shapeView shape "0" (2*model.unit)]
                                    Nothing ->
                                        []
                               )
                       ) 
              ]
         )
