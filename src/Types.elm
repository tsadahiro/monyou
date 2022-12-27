module Types exposing (..)

import ColorPicker
import Color exposing (Color)

type CrystalType = Rokkaku
                 | Asanoha
                 | Ichimatsu

type Msg = Down {x:Float, y:Float}
         | Up {x:Float, y:Float}
         | Move {x:Float, y:Float}
         | Click {x:Float, y:Float}
         | DoubleClick
         | Crystal CrystalType
         | FillColorPicked ColorPicker.Msg
         | LineColorPicked ColorPicker.Msg
         | ModeSelected EditMode
         | DeleteAll
         | DeleteShape Int
         | MoveUp Int
           
type alias Point = {x:Float
                   ,y:Float
                   }

type alias CircleData = {cx:Float
                        ,cy:Float
                        ,r:Float
                        ,fillColor:String
                        ,stroke:String
                        }

type alias OresenData = {points:List Point
                        ,stroke:String
                        }

type alias PolygonData = {points:List Point
                         ,stroke:String
                         ,fillColor:String
                         }

type Primitive = Oresen OresenData
               | Polygon PolygonData
               | Circle CircleData


type EditMode = CircleMode | LineMode | PolygonMode
                 
type alias Model = { shapes: List Primitive
                   , newShape: Maybe Primitive
                   , editMode: EditMode
                   , fillColor: Color
                   , lineColor: Color
                   , unit: Int
                   , monyou: CrystalType
                   , fillColorPicker : ColorPicker.State
                   , lineColorPicker : ColorPicker.State
                   , editorMargin : Int
                   }
                 
