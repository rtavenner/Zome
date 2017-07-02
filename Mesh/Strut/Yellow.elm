module Mesh.Strut.Yellow exposing (yellow)

import Phi.Num as P exposing (PhiNum,halve,neg,add,sub,less,mul,phith)

import Mesh.Shared exposing (..)

yellow : PhiNum -> Mesh
yellow n =
    let
        phi2th = mul phith phith
        phi3th = mul phith phi2th
        a1 = (                  halve ball_short,        halve ball_long,      ball_radius)
        b1 = (sub ball_radius (halve ball_short), P.zero                ,  sub ball_radius (halve ball_long))
        c1 = (                  halve ball_short, neg <| halve ball_long,      ball_radius)
        forward (x,y,z) = 
            ( add (mul (sub (halve (sub n phi3th)) ball_radius) phi2th) x
            , y
            , add      (sub (halve (sub n phi3th)) ball_radius)         z)
        a2 = forward a1
        b2 = forward b1
        c2 = forward c1

        fun (x,y,z) = (sub (mul n phi2th) x , neg y, sub n z)
        d1 = fun a1
        e1 = fun b1
        f1 = fun c1
        d2 = fun a2
        e2 = fun b2
        f2 = fun c2
    in 
        ( [(a2,e2,c2),(e2,c2,d2),(c2,d2,b2),(d2,b2,f2),(b2,f2,a2),(f2,a2,e2)
          ,(a1,b1,b2),(b2,a2,a1)
          ,(b1,c1,c2),(c2,b2,b1)
          ,(c1,a1,a2),(a2,c2,c1)
          ,(d1,e1,e2),(e2,d2,d1)
          ,(e1,f1,f2),(f2,e2,e1)
          ,(f1,d1,d2),(d2,f2,f1)
          ]
        , [(a2,e2),(e2,c2),(c2,d2),(d2,b2),(b2,f2),(f2,a2)
          ,(a1,a2),(b1,b2),(c1,c2),(d1,d2),(e1,e2),(f1,f2)]
        )
            
