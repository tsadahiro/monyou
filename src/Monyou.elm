module Monyou exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (Svg, svg, rect)
import Svg.Attributes as SAt
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Region exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Ev
import Element.Font as Font
import Element.Input as Input
import ColorPicker
import Color exposing (Color)
import Html.Events.Extra.Pointer exposing (..)
import Html.Events.Extra.Mouse as Mouse

import Asanoha
import Ichimatsu
import Rokkaku
import P3
import Editor
import Types exposing (..)

main = Browser.sandbox {init = init
                       ,update = update
                       ,view = view
                       }
    

init: Model
init = {unit = 70
       ,shapes = []
       ,newShape = Nothing
       ,editMode = CircleMode
       ,fillColor = (Color.rgb 255 200 200)
       ,lineColor = (Color.rgb 255 0 0)
       ,monyou = Asanoha
       ,fillColorPicker = ColorPicker.empty
       ,lineColorPicker = ColorPicker.empty
       ,editorMargin = 65
       }

update : Msg -> Model -> Model
update  msg model =
    case msg of
        Down pos ->
            case model.editMode of
                CircleMode ->
                    let
                        dummy = Debug.log "down" pos
                    in
                    {model | newShape = Just (Circle (CircleData
                                                          (pos.x/(toFloat model.unit)/4)
                                                          (pos.y/(toFloat model.unit)/4)
                                                          0
                                                          (Color.toCssString model.fillColor)
                                                          (Color.toCssString model.lineColor)
                                                     )
                                             )
                    }
                LineMode -> model
                PolygonMode -> model
        Up pos ->
            case model.editMode of
                CircleMode ->
                    let
                        dummy = Debug.log "up" pos
                        newlyAdded =
                            case model.newShape of
                                Just shape -> [shape]
                                Nothing -> []
                    in
                        {model|shapes = model.shapes++newlyAdded
                        ,newShape = Nothing}
                LineMode -> model
                PolygonMode -> model
        Move pos ->
            case model.editMode of
              CircleMode ->
                case model.newShape of
                  Just (Circle circledata) ->
                      let
                          newR = sqrt ((circledata.cx - (pos.x/(4*(toFloat model.unit))))^2
                                         +(circledata.cy - (pos.y/(4*(toFloat model.unit))))^2)
                      in
                          {model | newShape = Just (Circle {circledata|r=newR})}
                  _ ->
                      model
              LineMode ->
                case model.newShape of
                  Just (Oresen oresen) ->
                      let
                          newEndPoint = Point (pos.x/(4*(toFloat model.unit)))
                                              (pos.y/(4*(toFloat model.unit)))
                      in
                          {model | newShape = Just (Oresen {oresen |
                                                                points =
                                                                (List.take ((List.length oresen.points)-1) oresen.points)++
                                                                [newEndPoint]
                                                           }
                                                   )
                          }
                  _ ->
                      model
              PolygonMode ->
                case model.newShape of
                  Just (Polygon polygon) ->
                      let
                          newEndPoint = Point (pos.x/(4*(toFloat model.unit)))
                                              (pos.y/(4*(toFloat model.unit)))
                      in
                          {model | newShape = Just (Polygon {polygon | points =
                                                                 (List.take ((List.length polygon.points)-1) polygon.points)++
                                                                 [newEndPoint]
                                                            }
                                                   )
                          }
                  _ ->
                      model
        Click pos ->
          case model.editMode of
            CircleMode -> model
            LineMode ->
              case model.newShape of
                Nothing -> Debug.log "clicked" <| 
                    {model | newShape = Just (Oresen (OresenData [Point (pos.x/(toFloat model.unit)/4)
                                                                      (pos.y/(toFloat model.unit)/4)]
                                                          (Color.toCssString model.lineColor)
                                                     )
                                             )
                    }
                Just (Oresen oresen) -> {model | newShape = Just (Oresen {oresen | points = oresen.points ++
                                                                             [Point (pos.x/(toFloat model.unit)/4)
                                                                                  (pos.y/(toFloat model.unit)/4)
                                                                             ]
                                                                        }
                                                                 )
                                        }
                _ -> model
            PolygonMode ->
              case model.newShape of
                Nothing ->
                    {model | newShape = Just (Polygon (PolygonData [Point (pos.x/(toFloat model.unit)/4)
                                                                        (pos.y/(toFloat model.unit)/4)]
                                                           (Color.toCssString model.lineColor)
                                                           (Color.toCssString model.fillColor)
                                                      )
                                             )
                    }
                Just (Polygon polygon) ->
                    {model | newShape = Just (Polygon {polygon | points = polygon.points ++
                                                           [Point (pos.x/(toFloat model.unit)/4)
                                                                (pos.y/(toFloat model.unit)/4)
                                                           ]
                                                      }
                                             )
                    }
                _ -> model
        DoubleClick ->
          case model.editMode of
            CircleMode -> model
            LineMode ->
                case model.newShape of
                    Just oresen ->
                        {model | shapes = model.shapes ++ [oresen]
                        ,newShape = Nothing
                      }
                    Nothing -> model
            PolygonMode ->
              case model.newShape of
                  Just polygon ->
                      {model | shapes = model.shapes ++ [polygon]
                      ,newShape = Nothing
                      }
                  Nothing -> model
        Crystal crystalType ->
            {model|monyou = crystalType}
        ModeSelected editMode ->
            let
                dummy = Debug.log "mode" editMode
            in
                {model | editMode = editMode}
        FillColorPicked cmsg ->
            let
                ( m, col ) =
                    ColorPicker.update cmsg model.fillColor model.fillColorPicker
            in
                { model
                    | fillColorPicker = m
                    , fillColor = col |> Maybe.withDefault model.fillColor
                }
        LineColorPicked cmsg ->
            let
                ( m, col ) =
                    ColorPicker.update cmsg model.lineColor model.lineColorPicker
            in
                { model
                    | lineColorPicker = m
                    , lineColor = col |> Maybe.withDefault model.lineColor
                }
        DeleteAll -> {model | shapes = []}
        DeleteShape idx -> {model | shapes = (List.take (idx) model.shapes) ++ (List.drop (idx+1) model.shapes)}
        MoveUp idx -> {model | shapes = (List.take (idx-1) model.shapes) ++
                       (List.reverse <| List.drop (idx-1) <| List.take (idx+1) model.shapes) ++
                       (List.drop (idx+1) model.shapes)}

screen : Model -> Element Msg
screen model = html <| screenSvg model
                     
screenSvg : Model -> Svg Msg
screenSvg model = (svg [ SAt.width "700"
                       , SAt.height "700"
                       , SAt.id "screen"
                     ]
                   ([rect [SAt.width "100%"
                          ,SAt.height "100%"
                          ,SAt.fill "white"
                          ,SAt.stroke "black"
                          ][]
                    ]++(case model.monyou of
                             Asanoha -> Asanoha.rhombi model
                             Ichimatsu -> Ichimatsu.rhombi model
                             Rokkaku -> Rokkaku.rhombi model
                             P3 -> P3.rhombi model
                        )
                   )
              )

tools model =
    (column [spacing 10
            ,height fill
            ,Border.color color.blue
            ,Border.width 2
            ,Background.color color.lightGrey
            ,padding 10
            ]
         [el[padding 10
            ,Background.color color.white
            ,Border.color color.blue
            ,Border.width 2
            ]
              (row [spacing 10]
                   [text "save "
                   ,html (Html.div [Html.Attributes.id "buttonArea"][])
                   ]
              )
         ,el[padding 10
            ,Background.color color.white
            ,Border.color color.blue
            ,Border.width 2
            ]
              (row [spacing 10]
                   [text "紋様群"
                   ,button "p6" (Crystal Rokkaku) (model.monyou == Rokkaku)
                   ,button "p6mm" (Crystal Asanoha) (model.monyou == Asanoha)
                   ,button "p4mm" (Crystal Ichimatsu) (model.monyou == Ichimatsu)
                   ,button "p3" (Crystal P3) (model.monyou == P3)
                   ]
              )
         ,el[padding 10
            ,Background.color color.white
            ,Border.width 2
            ,Border.color color.blue
            ]
              (row [spacing 10]
                   [text "モード"
                   ,button "折れ線" (ModeSelected LineMode) (model.editMode == LineMode)
                   ,button "多角形" (ModeSelected PolygonMode) (model.editMode == PolygonMode)
                   ,button "円" (ModeSelected CircleMode) (model.editMode == CircleMode)
                   ]
              )
         ,row[spacing 10][
               column[][
                    el[padding 10
                      ,Background.color color.white
                      ,Border.width 2
                      ,Border.color color.blue
                      ]
                        (column [spacing 10]
                             [text "塗り色"
                             ,html (ColorPicker.view model.fillColor ColorPicker.empty
                                   |> Html.map FillColorPicked 
                                   )
                             ]
                        )
                   ,el[padding 10
                      ,Background.color color.white
                      ,Border.width 2
                      ,Border.color color.blue
                      ]
                        (column [spacing 10]
                             [text "線の色"
                             ,html (ColorPicker.view model.lineColor ColorPicker.empty
                                   |> Html.map LineColorPicked 
                                   )
                             ]
                        )
                   ]
              ,column [padding 10
                      ,spacing 10
                      ,Background.color color.white
                      ,Border.width 2
                      ,Border.color color.blue
                      ]
                   ([button "全消去" (DeleteAll) False]++(List.indexedMap shapeIconView (model.shapes)))
              ]
         ]
    )

shapeIconView : Int -> Primitive -> Element Msg
shapeIconView idx shape =
    row [Border.color color.blue
        ,spacing 10
        ]
        [case shape of 
             Circle d ->
                 el [Border.color color.blue]
                     <| html (svg [SAt.width "20"
                                ,SAt.height "20"][
                                   Svg.circle [SAt.cx "10"
                                              ,SAt.cy "10"
                                              ,SAt.r "10"
                                              ,SAt.fill d.fillColor
                                              ,SAt.stroke d.stroke
                                              ][]
                                  ])
             Oresen d ->
                 el [Border.color color.blue]
                     <| html (svg [SAt.width "20"
                                  ,SAt.height "20"]
                                  [
                                   Svg.path [SAt.d "M 5 5 l 10 5 l -5 5 l 10 5"
                                            ,SAt.stroke d.stroke
                                            ,SAt.fill "none"
                                            ][]
                                  ])
             Polygon d -> 
                 el [Border.color color.blue]
                     <| html (svg [SAt.width "20"
                                ,SAt.height "20"]
                                  [
                                   Svg.path [SAt.d "M 5 5 l 10 5 l -15 10 l 10 5 z"
                                            ,SAt.stroke d.stroke
                                            ,SAt.fill d.fillColor
                                            ][]
                                  ])
        ,button "🗑" (DeleteShape idx) False
        ,button "⬆" (MoveUp idx) False
        ]
view : Model -> Html Msg
view model =
    layout[]
        (row [spacing 20
             ,width fill
             ,padding 10
             ,Border.width 1
             ,Background.color color.white
             ]
             [tools model
             ,el[] (Editor.editor model)
             ,el[] (screen model)
             ]
        )

button : String -> Msg -> Bool -> Element Msg
button label msg selected = Input.button [padding 5
                                         , Background.color (if selected then
                                                                 color.blue
                                                             else
                                                                 color.lightBlue
                                                            )
                                         , Border.width 2
                                         , Border.rounded 6
                                         , Border.color color.lightBlue                                                        
                                         , Border.shadow
                                              { offset = ( 4, 4 ), size = 3, blur = 10, color = color.lightGrey }
                                         , Font.color (if selected then
                                                           color.lightBlue
                                                       else
                                                             color.blue
                                                      )
                                         , Ev.onClick  msg
                                         ]
                            {label = text label
                            ,onPress = Nothing
                            }
            
color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }



