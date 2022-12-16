module Editor exposing (..)

import Html.Events.Extra.Pointer exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Element exposing (..)
import Svg exposing (svg, rect)
import Svg.Attributes as SAt
import Asanoha
import Ichimatsu
import Rokkaku
import PrimitivesView exposing (..)
import Types exposing (..)


editor : Model -> Element Msg
editor model =
    let
        monyou = model.monyou
        relativePos : Event -> {x:Float, y:Float}
        relativePos event =
            {x = ((Tuple.first event.pointer.offsetPos) - (toFloat (4*model.editorMargin)))/4
            ,y = ((Tuple.second event.pointer.offsetPos) - (toFloat (4*model.editorMargin)))/4
            }
        mousePos : Mouse.Event -> {x:Float, y:Float}
        mousePos event =
            {x = ((Tuple.first event.offsetPos) - (toFloat (4*model.editorMargin)))/4
            ,y = ((Tuple.second event.offsetPos) - (toFloat (4*model.editorMargin)))/4
            }
            {-{x=Tuple.first event.offsetPos - (toFloat model.editorMargin*4)
            ,y=Tuple.second event.offsetPos - (toFloat model.editorMargin*4)
            }-}
    in
        html (svg [ SAt.width "600"
                  , SAt.height "600"
                  , onDown (relativePos >> Down)
                  , onUp (relativePos >> Up)
                  , onMove (relativePos >> Move)
                  , Mouse.onClick (mousePos >> Click)
                  , Mouse.onDoubleClick (\ev -> DoubleClick)
                  ]
                  [Svg.g[SAt.transform "scale(4,4) translate(10,10)"]
                       ([Svg.clipPath [SAt.id ("cell0")]
                             [case monyou of
                                  Asanoha -> Asanoha.fundPath ((toFloat model.unit))
                                  Ichimatsu -> Ichimatsu.fundPath ((toFloat model.unit))
                                  Rokkaku -> Rokkaku.fundPath ((toFloat model.unit))
                             ]
                        ,rect [SAt.width "150"
                              ,SAt.height "150"
                              ,SAt.x "-10"
                              ,SAt.y "-10"
                              ,SAt.fill "#eee"
                              ,SAt.stroke "black"
                              ][]
                        ,rect [SAt.width "100%"
                              ,SAt.height "100%"
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
         {-
svg [ Svg.Attributes.width "600"
                  ,Svg.Attributes.height "600"
                  ]
                 [Svg.g[transform "scale(4,4) translate(20,40)"]
                      (
                        [Svg.clipPath [Svg.Attributes.id ("cell0")]
                        (case model.crystal of
                          Asanoha -> [fundPathAsa ((toFloat model.unit)*0.25)]
                          Mutsude -> [fundPathMutsu ((toFloat model.unit)*0.25)]
                          Ichimatsu -> [fundPathIchi ((toFloat model.unit)*0.25)]
                          )
                             --[fundPathAsa ((toFloat model.unit)*0.25)]
                             --[fundPathMutsu ((toFloat model.unit)*0.25)]
                             --[fundPathIchi ((toFloat model.unit)*0.25)]
                        ,rect [Svg.Attributes.width "100%"
                              ,Svg.Attributes.height "100%"
                              ,Svg.Attributes.fill "#cef"
                              ,Svg.Attributes.clipPath ("url(#cell0)")
                              ][]
                        ,rect [Svg.Attributes.width "130"
                              ,Svg.Attributes.height "130"
                              ,x "-20"
                              ,y "-40"
                              ,Svg.Attributes.fill "none"
                              ,stroke "black"
                              ][]
                        ]++
                           (List.map (\circle -> circleView "0" circle (toFloat model.unit)) model.circles)
                           ++ (List.map (\oresen -> oresenView "0" oresen (toFloat model.unit)) model.oresens)
                           ++ (List.map (\polygon -> polygonView "0" polygon (toFloat model.unit)) model.polygons)
                           ++ (case model.newCircle of
                                   Just circle ->
                                       [circleView "0" circle (toFloat model.unit)]
                                   Nothing ->
                                       []
                              )
                           ++ (case model.newOresen of
                                   Just oresen ->
                                       [oresenView "0" oresen (toFloat model.unit)]
                                   Nothing ->
                                       []
                              )
                          ++ (case model.newPolygon of
                                  Just polygon ->
                                      [polygonView "0" polygon (toFloat model.unit)]
                                  Nothing ->
                                      []
                             )
                      )
                 ]
-}
