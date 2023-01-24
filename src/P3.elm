module P3 exposing (..)

import Svg as Svg exposing  (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Types exposing (..)
import PrimitivesView exposing (..)

type alias MData = {unit:Int
                   }


rhombi : Model -> List (Svg Msg)
rhombi model =
    let
        n=20
    in
        List.map (\i ->
                      let
                          s = i // n - n//2
                          t = modBy n i
                      in
                          rhombus model s t model.unit
                 )
            (List.range 0 (n*n-1))

rhombus: Model -> Int -> Int -> Int -> Svg Msg
rhombus model s t unit =
    let
        ox = 0.5*((toFloat unit)*3/2*(toFloat (s+t)) - (toFloat unit*6))
        oy = 0.5*((toFloat unit)*(sqrt 3)/2*(toFloat (s-t)) +(toFloat unit*18))
        prefix = (String.fromInt s) ++ "-" ++  (String.fromInt t)
    in
        Svg.g [transform ("translate(" ++ (String.fromFloat ox) ++ "," ++ (String.fromFloat oy) ++ ")")]
            [fundD (prefix ++ "a") model
            ,Svg.g [transform "rotate(120)"]
                [fundD (prefix ++ "b") model]
            ,Svg.g [transform "rotate(240)"]
                [fundD (prefix ++ "c") model]
            ]

                 
fundPath: Float -> Svg Msg
fundPath size =
    Svg.path [d ("M 0 0 l " ++
                (String.fromFloat (size/2)) ++ " " ++
                (String.fromFloat ((sqrt 3)/2*size)) ++
                    " l " ++
                    (String.fromFloat (size/2)) ++ " " ++
                    (String.fromFloat (-(sqrt 3)/2*size)) ++
                    " l " ++
                    (String.fromFloat (-size/2)) ++ " " ++
                    (String.fromFloat ((-(sqrt 3))/2*size)) ++ " " ++
                    " l " ++
                    (String.fromFloat (-size/2)) ++ " " ++
                    (String.fromFloat (((sqrt 3))/2*size)) ++ " " ++
                    " z")
            ,stroke "red"
            ,Svg.Attributes.fill "gray"]
       []

           
fundD: String -> Model ->Svg Msg
fundD id model =
    Svg.g []
        ([Svg.clipPath [Svg.Attributes.id ("cell" ++ id)
                      ]
             [fundPath ((toFloat model.unit))]
         ] ++
        [Svg.clipPath [Svg.Attributes.id ("cell" ++ id)
                      ]
             [fundPath ((toFloat model.unit))]
         ] ++ 
            (List.map (\shape -> shapeView shape id model.unit) model.shapes)
            ++
            (case model.newShape of
                 Just shape ->
                     [shapeView shape id model.unit]
                 Nothing ->
                     []
            )                
        )
            
