module Asanoha exposing (..)

rhombi : Model -> Int -> List (Svg Msg)
rhombi model n =
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
        ox = (toFloat unit)*((toFloat s) + (toFloat t)/2)
        oy = (toFloat unit)*sqrt(3)/2*(toFloat t)
        prefix = (String.fromInt s) ++ "-" ++  (String.fromInt t)
    in
        Svg.g [transform ("translate(" ++ (String.fromFloat ox) ++ "," ++ (String.fromFloat oy) ++ ")")]
            [fundDAsa (prefix ++ "a") model
            ,Svg.g [transform ("matrix(-1,0,0,1," ++ (String.fromFloat (toFloat model.unit)) ++ ",0)")]
                [fundDAsa (prefix ++ "b") model]
            ,Svg.g [transform "rotate(60)"]
                [
                 fundDAsa (prefix ++ "c")model
                ,Svg.g [transform ("matrix(-1,0,0,1," ++ (String.fromFloat (toFloat model.unit)) ++ ",0)")]
                    [fundDAsa (prefix ++ "d") model]
                ]
            ,Svg.g [transform "rotate(120)"]
                [
                 fundDAsa (prefix ++ "e") model
                ,Svg.g [transform ("matrix(-1,0,0,1," ++ (String.fromFloat (toFloat model.unit)) ++ ",0)")]
                    [fundDAsa (prefix ++ "e") model]
                ]
            ,Svg.g [transform "scale(1,-1)"]
                [
                 fundDAsa (prefix ++ "f") model
                ,Svg.g [transform ("matrix(-1,0,0,1," ++ (String.fromFloat (toFloat model.unit)) ++ ",0)")]
                    [fundDAsa (prefix ++ "g") model]
                ]
            ,Svg.g [transform " rotate(60) scale(1,-1)"]
                [
                 fundDAsa (prefix ++ "h")model
                ,Svg.g [transform ("matrix(-1,0,0,1," ++ (String.fromFloat (toFloat model.unit)) ++ ",0)")]
                    [fundDAsa (prefix ++ "i") model]
                ]
            ,Svg.g [transform " rotate(120) scale(1,-1)"]
                [
                 fundDAsa (prefix ++ "j") model
                ,Svg.g [transform ("matrix(-1,0,0,1," ++ (String.fromFloat (toFloat model.unit)) ++ ",0)")]
                    [fundDAsa (prefix ++ "k") model]
                ]
            ]
