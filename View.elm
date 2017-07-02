module View exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import WebGL

import Phi.Vec exposing (PhiVec)

import Entity exposing (ball,strut)


view : 
    { cam : Entity.Info
    , balls : List PhiVec
    , struts : List (PhiVec, PhiVec)
    }
    -> Html msg
view {cam,balls,struts} =
    WebGL.toHtmlWith 
        [ WebGL.alpha True, WebGL.antialias, WebGL.depth 1 -- default
        , WebGL.clearColor 0 0 0 1]  -- mine
        [ width  900
        , height 900
        , style [ ( "display", "block" ) ]
        ]
        (  List.concatMap (ball  cam) balls
        ++ List.concatMap (strut cam) struts)



