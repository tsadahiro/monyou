module PrimitivesView exposing (..)

import Types exposing (..)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes as SAt exposing (cx, cy, r, stroke, d)


circleView: String -> CircleData -> Float -> Svg Msg
circleView id circle unit =
    Svg.circle [cx (String.fromFloat (unit*circle.cx))
               ,cy (String.fromFloat (unit*circle.cy))
               ,r  (String.fromFloat (unit*circle.r))
               ,SAt.fill circle.fillColor
               ,stroke circle.stroke
               ,SAt.clipPath ("url(#cell" ++ id ++ ")")
               ][]

oresenView: String -> OresenData -> Float -> Svg Msg
oresenView id oresen unit =
    let
        pathString = List.foldl (\point str -> (str ++
                                (if str == "" then
                                           "M "
                                 else
                                   "L ")++
                                (String.fromFloat (unit*point.x)) ++ " " ++
                                (String.fromFloat (unit*point.y)) ++ " ")
                                ) "" oresen.points
    in

    Svg.path [d pathString
             ,SAt.fill "none"
             ,stroke oresen.stroke
             ,SAt.clipPath ("url(#cell" ++ id ++ ")")
             ][]

polygonView: String -> PolygonData -> Float -> Svg Msg
polygonView id polygon unit =
    let
        pathString = List.foldl (\point str -> (str ++
                                (if str == "" then
                                           "M "
                                 else
                                   "L ")++
                                (String.fromFloat (unit*point.x)) ++ " " ++
                                (String.fromFloat (unit*point.y)) ++ " ")
                                ) "" polygon.points
    in

    Svg.path [d (pathString ++ " Z ")
             ,SAt.fill polygon.fillColor
             ,stroke polygon.stroke
             ,SAt.clipPath ("url(#cell" ++ id ++ ")")
             ][]
    

shapeView : Primitive -> String -> Int -> Svg Msg
shapeView shape id unit =
    case shape of
        Oresen oresen -> oresenView id oresen ((toFloat unit)*2)
        Polygon polygon -> polygonView id polygon ((toFloat unit)*2)
        Circle circle -> circleView id circle ((toFloat unit)*2)
        
