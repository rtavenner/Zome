module Main exposing (main)

import Html exposing (Html)
import Math.Matrix4 as Mat4 exposing (Mat4,makeRotate)
import Math.Vector3 as Vec3 exposing (vec3)

import Phi.Num exposing (zero, one, neg, phi2th)
import Phi.Vec exposing (transform)

import View

import Mouse exposing (Position)

type alias Model = {mousePos : Maybe Position , rot : Mat4}
type Msg 
    = MouseMove Position
    | MouseDown Position
    | MouseUp

main : Program Never Model Msg
main =
    Html.program
        { init = ( {mousePos = Nothing, rot = Mat4.identity}, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        , update = update}

subscriptions : Model -> Sub Msg
subscriptions _ = 
    Sub.batch
        [ Mouse.moves MouseMove
        , Mouse.downs MouseDown
        , Mouse.ups (always MouseUp)
        ]

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        MouseMove pos ->
            case model.mousePos of
                Nothing -> model ! []
                Just oldpos ->
                    ( { model
                      | mousePos = Just pos
                      }
                      |> mouseDrag (pos.x - oldpos.x, pos.y - oldpos.y)
                    ) ! []
        MouseDown pos ->
            {model | mousePos = Just pos} ! []
        MouseUp ->
            {model | mousePos = Nothing} ! []


mouseDrag : (Int,Int) -> Model -> Model
mouseDrag (dx,dy) model =
    if dx == 0 && dy == 0
    then model
    else 
        let x = toFloat dx / 100
            y = toFloat dy / 100
        in  { model
            | rot =
                Mat4.mul
                    (makeRotate (sqrt (x^2+y^2)) (vec3 y x 0))
                    model.rot}

info : Model -> { camera : Mat4, perspective : Mat4, rot : Mat4 }
info model = 
    { perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 7) (vec3 0 0 0) (vec3 0 1 0)
    , rot = model.rot}

view : Model -> Html msg
view model =
    View.view
        { cam = info model
        , balls = balls
        , struts = struts}

balls = 
    List.concatMap
        (\s -> [transform s (one,one,one), transform s (phi2th,phi2th,phi2th)])
        (Phi.Vec.combineSymms Phi.Vec.symmXYZ Phi.Vec.symmRefl)

struts =
    List.concatMap
        (\s -> 
            [ (transform s (one,one,one), transform s (neg one,one,one))
            , (transform s (phi2th,phi2th,phi2th), transform s (neg phi2th,phi2th,phi2th))])
        Phi.Vec.tetrahedral
    ++ List.map
        (\s -> (transform s (phi2th,phi2th,phi2th), transform s (one,one,one)))
        (Phi.Vec.combineSymms Phi.Vec.symmXYZ Phi.Vec.symmRefl)
