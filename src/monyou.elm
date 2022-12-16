module Monyou exposing(..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing()

main = Browser.sandbox {init=init, update=update, view=view}

init: Int
init = 0

update: Msg -> Model -> Model
update msg model =
    model

view: Model -> Html Msg
view model =
    div[][]
    
