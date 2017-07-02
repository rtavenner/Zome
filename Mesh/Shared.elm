module Mesh.Shared exposing (..)

import Phi.Num as P exposing (PhiNum)
import Phi.Vec exposing (PhiVec)

type alias Edge = (PhiVec,PhiVec)
type alias Tri = (PhiVec,PhiVec,PhiVec)
type alias Mesh = (List Tri, List Edge)


withSymm : Phi.Vec.Symmetries -> Mesh -> Mesh
withSymm symm (a,b) = 
    ( List.concatMap 
        ((\f (a,b,c) -> List.map3 (,,) (f a) (f b) (f c)) (Phi.Vec.transformAll symm))
        a 
    , List.concatMap 
        ((\f (a,b) -> List.map2 (,) (f a) (f b)) (Phi.Vec.transformAll symm))
        b
    )

trimap : (a -> b) -> ( a, a, a ) -> ( b, b, b )
trimap f (a,b,c) = (f a,f b,f c)

bimap : (a -> b) -> ( a, a ) -> ( b, b )
bimap f (a,b) = (f a,f b)

ball_radius : PhiNum
ball_radius = P.phith |> P.mul P.phith |> P.mul P.phith |> P.mul P.phith

ball_long : PhiNum
ball_long = P.mul ball_radius P.phith

ball_short : PhiNum
ball_short = P.mul ball_long P.phith


