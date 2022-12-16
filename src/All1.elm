module All1 exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Browser
import Browser.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html.Events.Extra.Pointer exposing (..)
import ColorPicker
import Color exposing (Color)
import Html.Events.Extra.Mouse as Mouse
import Element exposing (..)
import Element.Region exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

main = Browser.sandbox {init=init
                       ,update=update
                       ,view=view}

type alias Circle = {cx:Float
                    ,cy:Float
                    ,r:Float
                    ,fillColor:String
                    ,stroke:String
                    }

type alias Point = {x:Float
                   ,y:Float
                   }

type alias Oresen = {points:List Point
                    ,stroke:String
                    }

type alias Polygon = {points:List Point
                     ,stroke:String
                     ,fillColor:String
                   }

type Mode = CircleMode | LineMode | PolygonMode

type Crystal = Asanoha | Mutsude | Ichimatsu

type alias Model = {unit:Int
                   ,circles: List Circle
                   ,newCircle: Maybe Circle
                   ,oresens: List Oresen
                   ,newOresen: Maybe Oresen
                   ,polygons: List Polygon
                   ,newPolygon: Maybe Polygon
                   ,colorPicker : ColorPicker.State
                   ,linecolorPicker : ColorPicker.State
                   ,color : Color.Color
                   ,linecolor : Color.Color
                   ,editMode : Mode
                   ,crystal : Crystal
                   }
    
type Msg = Down {x:Float, y:Float}
    | Up {x:Float, y:Float}
    | Move {x:Float, y:Float}
    | ColorPicked ColorPicker.Msg
    | LineColorPicked ColorPicker.Msg
    | ToCircleMode
    | ToLineMode
    | ToPolygonMode
    | ToAsanoha
    | ToMutsude
    | ToIchimatsu
    | Click {x:Float, y:Float}
    | DoubleClick

init: Model
init = Model 200
      []
      Nothing
      []
      Nothing
      []
      Nothing
      ColorPicker.empty
      ColorPicker.empty
      (Color.rgb 255 200 200)
      (Color.rgb 255 200 200)
      CircleMode
      Asanoha


update: Msg -> Model -> Model
update msg model =
    case msg of
        Down pos ->
            let
                dummy = Debug.log "down" pos
            in
                case model.editMode of
                  CircleMode ->
                          {model | newCircle = Just (Circle (pos.x/(toFloat model.unit)/4)
                                               (pos.y/(toFloat model.unit)/4)
                                               0 (Color.toCssString model.color) (Color.toCssString model.linecolor))}
                  LineMode -> model
                  PolygonMode -> model

        Up pos ->
          case model.editMode of
            CircleMode ->
              let
                  dummy = Debug.log "up" pos
                  newlyAdded =
                      case model.newCircle of
                          Just circle -> [circle]
                          Nothing -> []
              in
                 {model|circles = model.circles++newlyAdded
                 ,newCircle = Nothing}
            LineMode -> model
            PolygonMode -> model
        Move pos ->
            case model.editMode of
              CircleMode ->
                case model.newCircle of
                  Just circle ->
                      let
                          newR = sqrt ((circle.cx - (pos.x/(4*(toFloat model.unit))))^2
                                         +(circle.cy - (pos.y/(4*(toFloat model.unit))))^2)
                      in
                          {model | newCircle = Just {circle|r=newR}}
                  Nothing ->
                      model
              LineMode ->
                case model.newOresen of
                  Just oresen ->
                      let
                          newEndPoint = Point (pos.x/(4*(toFloat model.unit)))
                                              (pos.y/(4*(toFloat model.unit)))
                      in
                          {model | newOresen = Just {oresen |
                                                         points =
                                                         (List.take ((List.length oresen.points)-1) oresen.points)++
                                                         [newEndPoint]
                                                    }
                          }
                  Nothing ->
                      model
              PolygonMode ->
                case model.newPolygon of
                  Just polygon ->
                      let
                          newEndPoint = Point (pos.x/(4*(toFloat model.unit)))
                                              (pos.y/(4*(toFloat model.unit)))
                      in
                          {model | newPolygon = Just {polygon | points =
                                                          (List.take ((List.length polygon.points)-1) polygon.points)++
                                                          [newEndPoint]
                                                     }
                          }
                  Nothing ->
                      model
        ColorPicked cmsg ->
            let
                ( m, color ) =
                    ColorPicker.update cmsg model.color model.colorPicker
            in
                { model
                    | colorPicker = m
                    , color = color |> Maybe.withDefault model.color
                }
        LineColorPicked cmsg ->
            let
                ( m, color ) =
                    ColorPicker.update cmsg model.linecolor model.linecolorPicker
            in
                { model
                    | colorPicker = m
                    , linecolor = color |> Maybe.withDefault model.linecolor
                }
        ToCircleMode ->
          {model | editMode = CircleMode}
        ToLineMode ->
          {model | editMode = LineMode}
        ToPolygonMode ->
          {model | editMode = PolygonMode}
        ToAsanoha ->
          {model | crystal = Asanoha}
        ToMutsude ->
          {model | crystal = Mutsude}
        ToIchimatsu ->
          {model | crystal = Ichimatsu}
        Click pos ->
          case model.editMode of
            CircleMode -> model
            LineMode ->
              case model.newOresen of
                Nothing -> {model | newOresen = Just (Oresen [Point (pos.x/(toFloat model.unit)/4)
                                (pos.y/(toFloat model.unit)/4)]
                                (Color.toCssString model.linecolor)
                                )}
                Just oresen -> {model | newOresen = Just {oresen | points = oresen.points ++
                                        [Point (pos.x/(toFloat model.unit)/4)
                                               (pos.y/(toFloat model.unit)/4)
                                        ]
                                      }
                               }
            PolygonMode ->
              case model.newPolygon of
                Nothing -> {model | newPolygon = Just (Polygon [Point (pos.x/(toFloat model.unit)/4)
                                (pos.y/(toFloat model.unit)/4)]
                                (Color.toCssString model.linecolor)
                                (Color.toCssString model.color)
                                )}
                Just polygon -> {model | newPolygon = Just {polygon | points = polygon.points ++
                                        [Point (pos.x/(toFloat model.unit)/4)
                                               (pos.y/(toFloat model.unit)/4)
                                        ]
                                      }
                               }
        DoubleClick ->
          case model.editMode of
            CircleMode -> model
            LineMode ->
                case model.newOresen of
                    Just oresen ->
                        {model | oresens = model.oresens ++ [oresen]
                        ,newOresen = Nothing
                      }
                    Nothing -> model
            PolygonMode ->
              case model.newPolygon of
                  Just polygon ->
                      {model | polygons = model.polygons ++ [polygon]
                      ,newPolygon = Nothing
                    }
                  Nothing -> model


rhombiAsa : Model -> Int -> List (Svg Msg)
rhombiAsa model n =
    List.map (\i ->
                  let
                      s = i // n - n//2
                      t = modBy n i
                  in
                      rhombusAsa model s t model.unit
             )
        (List.range 0 (n*n-1))

rhombusAsa: Model -> Int -> Int -> Int -> Svg Msg
rhombusAsa model s t unit =
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
rhombiMutsu : Model -> Int -> List (Svg Msg)
rhombiMutsu model n =
    List.map (\i ->
                  let
                      s = i // n - n//2
                      t = modBy n i
                  in
                      rhombusMutsu model s t model.unit
             )
        (List.range 0 (n*n-1))

rhombusMutsu: Model -> Int -> Int -> Int -> Svg Msg
rhombusMutsu model s t unit =
    let
        ox = (toFloat unit)*((toFloat s) + (toFloat t)/2)
        oy = (toFloat unit)*sqrt(3)/2*(toFloat t)
        prefix = (String.fromInt s) ++ "-" ++  (String.fromInt t)
    in
        Svg.g [transform ("translate(" ++ (String.fromFloat ox) ++ "," ++ (String.fromFloat oy) ++ ")")]
            [fundDMutsu (prefix ++ "a") model
            ,Svg.g [transform ("translate(" ++ (String.fromInt (unit//2)) ++
              " " ++ (String.fromFloat (0.5/sqrt(3)*(toFloat model.unit))) ++ ")" ++
                "rotate(120) translate(" ++ (String.fromInt (-unit//2)) ++
              " " ++ (String.fromFloat (-0.5/sqrt(3)*(toFloat model.unit))) ++ ")")
              ]
                [fundDMutsu (prefix ++ "c")model]
            ,Svg.g [transform ("translate(" ++ (String.fromInt (unit//2)) ++
              " " ++ (String.fromFloat (0.5/sqrt(3)*(toFloat model.unit))) ++ ")" ++
                "rotate(240) translate(" ++ (String.fromInt (-unit//2)) ++
              " " ++ (String.fromFloat (-0.5/sqrt(3)*(toFloat model.unit))) ++ ")")
              ]
                [fundDMutsu (prefix ++ "e") model]
            ,Svg.g [transform "rotate(60)"]
            [fundDMutsu (prefix ++ "a") model
            ,Svg.g [transform ("translate(" ++ (String.fromInt (unit//2)) ++
              " " ++ (String.fromFloat (0.5/sqrt(3)*(toFloat model.unit))) ++ ")" ++
                "rotate(120) translate(" ++ (String.fromInt (-unit//2)) ++
              " " ++ (String.fromFloat (-0.5/sqrt(3)*(toFloat model.unit))) ++ ")")
              ]
                [fundDMutsu (prefix ++ "c")model]
            ,Svg.g [transform ("translate(" ++ (String.fromInt (unit//2)) ++
              " " ++ (String.fromFloat (0.5/sqrt(3)*(toFloat model.unit))) ++ ")" ++
                "rotate(240) translate(" ++ (String.fromInt (-unit//2)) ++
              " " ++ (String.fromFloat (-0.5/sqrt(3)*(toFloat model.unit))) ++ ")")
              ]
                [fundDMutsu (prefix ++ "e") model]]
            ]
rhombiIchi : Model -> Int -> List (Svg Msg)
rhombiIchi model n =
    List.map (\i ->
                  let
                      s = i // n - n//2
                      t = modBy n i
                  in
                      rhombusIchi model s t model.unit
             )
        (List.range 0 (n*n-1))

rhombusIchi: Model -> Int -> Int -> Int -> Svg Msg
rhombusIchi model s t unit =
      let
          ox = (toFloat unit)*(toFloat s)
          oy = (toFloat unit)*(toFloat t)
          prefix = (String.fromInt s) ++ "-" ++  (String.fromInt t)
      in
          Svg.g [transform ("translate(" ++ (String.fromFloat ox) ++ "," ++ (String.fromFloat oy) ++ ")")]
              [fundDIchi (prefix ++ "a") model
              ,Svg.g [transform ("matrix(-1,0,0,1," ++  "0,0)")]
                  [fundDIchi (prefix ++ "b") model]
              ,Svg.g [transform "rotate(90)"]
                  [fundDIchi (prefix ++ "a") model]
              ,Svg.g [transform ("matrix(1,0,0,-1,0,0)")]
                  [fundDIchi (prefix ++ "b") model]
              ,Svg.g [transform "rotate(180)"]
                  [fundDIchi (prefix ++ "a") model]
              ,Svg.g [transform "rotate(270)"]
                  [fundDIchi (prefix ++ "a") model]
              ,Svg.g [transform "matrix(0,1,1,0,0,0)"]
                  [fundDIchi (prefix ++ "b") model]
              ,Svg.g [transform "matrix(0,1,1,0,0,0)rotate(180)"]
                  [fundDIchi (prefix ++ "b") model]
              ]


circleView: String -> Circle -> Float -> Svg Msg
circleView id circle unit =
    Svg.circle [cx (String.fromFloat (unit*circle.cx))
               ,cy (String.fromFloat (unit*circle.cy))
               ,r  (String.fromFloat (unit*circle.r))
               ,Svg.Attributes.fill circle.fillColor
               ,stroke circle.stroke
               ,Svg.Attributes.clipPath ("url(#cell" ++ id ++ ")")
               ][]

oresenView: String -> Oresen -> Float -> Svg Msg
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
             ,Svg.Attributes.fill "none"
             ,stroke oresen.stroke
             ,Svg.Attributes.clipPath ("url(#cell" ++ id ++ ")")
             ][]

polygonView: String -> Polygon -> Float -> Svg Msg
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
             ,Svg.Attributes.fill polygon.fillColor
             ,stroke polygon.stroke
             ,Svg.Attributes.clipPath ("url(#cell" ++ id ++ ")")
             ][]

fundPathAsa: Float -> Svg Msg
fundPathAsa size =
    Svg.path [d ("M 0 0 l " ++
                (String.fromFloat size) ++ " " ++
                (String.fromFloat (1/sqrt(3)*size)) ++ " l 0" ++
                (String.fromFloat (-1/sqrt(3)*size)) ++ " l " ++
                (String.fromFloat (-size)) ++ " 0 z")
            ,stroke "red"
            ,Svg.Attributes.fill "gray"]
       []

fundDAsa: String -> Model ->Svg Msg
fundDAsa id model =
    Svg.g []
        ([Svg.clipPath [Svg.Attributes.id ("cell" ++ id)
                      ]
             [fundPathAsa ((toFloat model.unit)*0.5)]
         ] ++
        [Svg.clipPath [Svg.Attributes.id ("cell" ++ id)
                      ]
             [fundPathMutsu ((toFloat model.unit)*0.5)]
         ] ++ (List.map (\circle -> circleView id circle ((toFloat model.unit)*2)) model.circles)
           ++ (List.map (\oresen -> oresenView id oresen ((toFloat model.unit)*2)) model.oresens)
           ++ (List.map (\polygon -> polygonView id polygon ((toFloat model.unit)*2)) model.polygons)
             ++ (case model.newCircle of
                     Just circle ->
                         [circleView id circle ((toFloat model.unit)*2)]
                     Nothing ->
                         []
                )
             ++ (case model.newOresen of
                     Just oresen ->
                         [oresenView id oresen ((toFloat model.unit)*2)]
                     Nothing ->
                         []
                )
            ++ (case model.newPolygon of
                    Just polygon ->
                        [polygonView id polygon ((toFloat model.unit)*2)]
                    Nothing ->
                        []
               )
        )

fundPathMutsu: Float -> Svg Msg
fundPathMutsu size =
  Svg.path [d ("M 0 0 l " ++
               (String.fromFloat size) ++ " " ++
               (String.fromFloat (size/sqrt(3))) ++ " l " ++
               (String.fromFloat size)++ " " ++
               (String.fromFloat (-size/sqrt(3))) ++ " z ")
           ,stroke "red"
           ,Svg.Attributes.fill "gray"]
       []

fundDMutsu: String -> Model ->Svg Msg
fundDMutsu id model =
    Svg.g []
        ([Svg.clipPath [Svg.Attributes.id ("cell" ++ id)
                      ]
             [fundPathMutsu ((toFloat model.unit)*0.5)]
         ] ++ (List.map (\circle -> circleView id circle ((toFloat model.unit)*2)) model.circles)
           ++ (List.map (\oresen -> oresenView id oresen ((toFloat model.unit)*2)) model.oresens)
           ++ (List.map (\polygon -> polygonView id polygon ((toFloat model.unit)*2)) model.polygons)
             ++ (case model.newCircle of
                     Just circle ->
                         [circleView id circle ((toFloat model.unit)*2)]
                     Nothing ->
                         []
                )
             ++ (case model.newOresen of
                     Just oresen ->
                         [oresenView id oresen ((toFloat model.unit)*2)]
                     Nothing ->
                         []
                )
            ++ (case model.newPolygon of
                    Just polygon ->
                        [polygonView id polygon ((toFloat model.unit)*2)]
                    Nothing ->
                        []
               )
        )

fundPathIchi: Float -> Svg Msg
fundPathIchi size =
  Svg.path [d ("M 0 0 l " ++
               (String.fromFloat size) ++ " " ++
               (String.fromFloat size) ++ " l " ++
               (String.fromFloat -size) ++ " 0 z"
              )
           ,stroke "blue"
           ,Svg.Attributes.fill "gray"]
       []

fundDIchi: String -> Model ->Svg Msg
fundDIchi id model =
    Svg.g []
        ([Svg.clipPath [Svg.Attributes.id ("cell" ++ id)
                      ]
             [fundPathIchi ((toFloat model.unit)*0.5)]
         ] ++ (List.map (\circle -> circleView id circle ((toFloat model.unit)*2)) model.circles)
           ++ (List.map (\oresen -> oresenView id oresen ((toFloat model.unit)*2)) model.oresens)
           ++ (List.map (\polygon -> polygonView id polygon ((toFloat model.unit)*2)) model.polygons)
             ++ (case model.newCircle of
                     Just circle ->
                         [circleView id circle ((toFloat model.unit)*2)]
                     Nothing ->
                         []
                )
             ++ (case model.newOresen of
                     Just oresen ->
                         [oresenView id oresen ((toFloat model.unit)*2)]
                     Nothing ->
                         []
                )
            ++ (case model.newPolygon of
                    Just polygon ->
                        [polygonView id polygon ((toFloat model.unit)*2)]
                    Nothing ->
                        []
               )
        )


view: Model -> Html Msg
view model =
    Html.div[][
        Element.layout [] myRowOfStuff
         ,Html.div []
             [Html.h1[][Html.text "文様エディタ"]
             ,Html.div[]
                [Html.button[Html.Attributes.style "background"
                                                (case model.crystal of
                                                  Asanoha -> "green"
                                                  _ -> "white")
                            ,Html.Events.onClick ToAsanoha][Html.text "Asanoha"]
                ,Html.button[Html.Attributes.style "background"
                                                (case model.crystal of
                                                  Mutsude -> "green"
                                                  _ -> "white")
                            ,Html.Events.onClick ToMutsude][Html.text "Mutsude"]
                ,Html.button[Html.Attributes.style "background"
                                               (case model.crystal of
                                                 Ichimatsu -> "green"
                                                 _ -> "white")
                            ,Html.Events.onClick ToIchimatsu][Html.text "Ichimatsu"]
                       ]
             ,svg [ Svg.Attributes.width "800"
                  ,Svg.Attributes.height "800"
                  ,transform "matrix(1,0,0,-1,0,0)"]
                  (
                   [rect [Svg.Attributes.width "100%"
                         ,Svg.Attributes.height "100%"
                         ,Svg.Attributes.fill "#cef"][]
                   ]++
                     (case model.crystal of
                       Asanoha -> rhombiAsa model 10
                       Mutsude -> rhombiMutsu model 10
                       Ichimatsu -> rhombiIchi model 10)
                       --(rhombiAsa model 10)
                       --(rhombiMutsu model 10)
                       --(rhombiIchi model 10)
                  )
             ]
        ,Html.div[Html.Attributes.style "position" "absolute"
                 ,Html.Attributes.style "left" "800px"
                 ,Html.Attributes.style "top" "0px"]
             [Html.button[Html.Attributes.style "background"
                                                (case model.editMode of
                                                  CircleMode -> "green"
                                                  _ -> "white")
                          ,Html.Events.onClick ToCircleMode][Html.text "circle"]
             ,Html.button[Html.Attributes.style "background"
                                                (case model.editMode of
                                                  LineMode -> "green"
                                                  _ -> "white")
                          ,Html.Events.onClick ToLineMode][Html.text "line"]
            ,Html.button[Html.Attributes.style "background"
                                               (case model.editMode of
                                                 PolygonMode -> "green"
                                                 _ -> "white")
                         ,Html.Events.onClick ToPolygonMode][Html.text "polygon"]
             ,ColorPicker.view model.color model.colorPicker |> Html.map ColorPicked
             ,ColorPicker.view model.linecolor model.linecolorPicker |> Html.map LineColorPicked
             ,svg [ Svg.Attributes.width "600"
                  ,Svg.Attributes.height "600"
                  ,onDown (relativePos >> Down)
                  ,onUp (relativePos >> Up)
                  ,onMove (relativePos >> Move)
                  ,Mouse.onClick (mousePos >> Click)
                  ,Mouse.onDoubleClick (\ev -> DoubleClick)
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
             ]
        ]

myRowOfStuff : Element Msg
myRowOfStuff =
  column [Element.width Element.fill, Element.height Element.fill][
      row [Element.width Element.fill, centerY, Element.spacing 30]
          [
          html (Html.div[][Html.text "text html"])
          ]
      ,row [Element.width Element.fill, centerY, Element.spacing 30]
           [
           ]
  ]

relativePos : Event -> {x:Float, y:Float}
relativePos event =
    {x=Tuple.first event.pointer.offsetPos - (20*4)
    ,y=Tuple.second event.pointer.offsetPos - (40*4)
    }
mousePos : Mouse.Event -> {x:Float, y:Float}
mousePos event =
    {x=Tuple.first event.offsetPos - (20*4)
    ,y=Tuple.second event.offsetPos - (40*4)
    }
