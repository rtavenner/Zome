module Mesh.Strut.Blue exposing (blue)

import Phi.Num as P exposing (PhiNum,halve,neg,sub,less)

import Mesh.Shared exposing (..)

blue : PhiNum -> Mesh
blue l =
    if less ball_radius (halve l)
    then 
        let a1 = (       halve ball_short,        halve ball_long,       ball_radius)
            b1 = (       halve ball_short, neg <| halve ball_long,       ball_radius)
            c1 = (neg <| halve ball_short, neg <| halve ball_long,       ball_radius)
            d1 = (neg <| halve ball_short,        halve ball_long,       ball_radius)
            a2 = (       halve ball_short,        halve ball_long, sub l ball_radius)
            b2 = (       halve ball_short, neg <| halve ball_long, sub l ball_radius)
            c2 = (neg <| halve ball_short, neg <| halve ball_long, sub l ball_radius)
            d2 = (neg <| halve ball_short,        halve ball_long, sub l ball_radius)
        in 
            ( [ (a1,b1,b2), (b2,a2,a1)
              , (b1,c1,c2), (c2,b2,b1)
              , (c1,d1,d2), (d2,c2,c1)
              , (d1,a1,a2), (a2,d2,d1)
              ]
            , [ (a1,a2)
              , (b1,b2)
              , (c1,c2)
              , (d1,d2)
              ])
    else ([],[])
