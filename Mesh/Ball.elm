module Mesh.Ball exposing (ball)

import Phi.Num as P exposing (halve,neg,sub)
import Phi.Vec exposing (combineSymms)

import Mesh.Shared exposing (..)

(+++) : Mesh -> Mesh -> Mesh
(+++) (a,b) (c,d) = (a++c, b++d)

ball : Mesh
ball = (rects +++ tris +++ pents)

tris : Mesh
tris = 
    ( [ ( (       halve ball_long,     ball_radius                   , halve ball_short)
        , (P.zero                , sub ball_radius (halve ball_short), sub ball_radius (halve ball_long))
        , (neg <| halve ball_long,     ball_radius                   , halve ball_short)
        )
      ]
    , [])
    |> withSymm (Phi.Vec.symmY |> combineSymms Phi.Vec.symmRed |> combineSymms Phi.Vec.symmRefl)

pents : Mesh
pents =
    let a = (       halve ball_short, halve ball_long                   ,     ball_radius)
        b = (       halve ball_long ,       ball_long                   , sub ball_radius (halve ball_short))
        c = (P.zero                 , sub ball_radius (halve ball_short), sub ball_radius (halve ball_long))
        d = (neg <| halve ball_long ,       ball_long                   , sub ball_radius (halve ball_short))
        e = (neg <| halve ball_short, halve ball_long                   ,     ball_radius)
    in ([(a,b,c),(a,c,d),(a,d,e)],[])
    |> withSymm Phi.Vec.tetrahedral


rects : Mesh
rects =
    let
        a = (       halve ball_short,        halve ball_long, ball_radius)
        b = (neg <| halve ball_short,        halve ball_long, ball_radius)
        c = (neg <| halve ball_short, neg <| halve ball_long, ball_radius)
    in
            
    ([(a,b,c)],[(a,b),(b,c)])
    |> withSymm Phi.Vec.icosahedral
