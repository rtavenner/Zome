module Phi.Vec exposing (..)

import Phi.Num exposing (PhiNum,add,sub,mul,div,zero,one,half,phi,less,equal)


type alias PhiVec = (PhiNum,PhiNum,PhiNum)

type alias PhiMat = (PhiVec,PhiVec,PhiVec)

dot : PhiVec -> PhiVec -> PhiNum
dot (a,b,c) (d,e,f) = add (mul a d) (add (mul b e) (mul c f))

negateVec : PhiVec -> PhiVec
negateVec (a,b,c) = (sub zero a, sub zero b, sub zero c)

makeBasis : PhiVec -> PhiVec -> PhiVec -> PhiMat
makeBasis a b c = transpose (a,b,c)

transform : PhiMat -> PhiVec -> PhiVec
transform (a,b,c) v = (dot a v, dot b v, dot c v)

transpose : PhiMat -> PhiMat
transpose ((a,b,c),(d,e,f),(g,h,i)) = ((a,d,g),(b,e,h),(c,f,i))

mulMats : PhiMat -> PhiMat-> PhiMat
mulMats (a,b,c) = 
    transpose >>
    \(d,e,f) ->
        ( (dot a d, dot a e, dot a f)
        , (dot b d, dot b e, dot b f)
        , (dot c d, dot c e, dot c f))

i : PhiVec
i = (one,zero,zero)
j : PhiVec
j = (zero,one,zero)
k : PhiVec
k = (zero,zero,one)

id : PhiMat
id = (i,j,k)

reflOrigin : PhiMat
reflOrigin = (negateVec i, negateVec j, negateVec k)
rotateX : PhiMat
rotateX = (i, negateVec j, negateVec k)
rotateY : PhiMat
rotateY = (negateVec i, j, negateVec k)
rotateZ : PhiMat
rotateZ = (negateVec i, negateVec j, k)

type alias Symmetries = List PhiMat

transformAll : Symmetries -> PhiVec -> List PhiVec
transformAll s v = List.map (flip transform v) s

combineSymms : Symmetries -> Symmetries -> Symmetries
combineSymms a b = List.concatMap (\a -> List.map (mulMats a) b) a


symmRefl : Symmetries
symmRefl = [id, reflOrigin]

symmX : Symmetries
symmX = [id, rotateX]
symmY : Symmetries
symmY = [id, rotateY]
symmZ : Symmetries
symmZ = [id, rotateZ]

symmXYZ : Symmetries
symmXYZ = combineSymms symmX symmY

symmYellow : Symmetries
symmYellow = [id, (j,k,i), (k,i,j)]

symmRed : Symmetries
symmRed = 
    let halfmat = ((half,zero,zero),(zero,half,zero),(zero,zero,half))
        rot1 =
            mulMats halfmat 
                (makeBasis 
                    (sub phi  one, phi        , sub zero one)
                    (sub zero phi, one        , sub phi  one)
                    (one         , sub phi one,          phi))
        rot2 = mulMats rot1 rot1
    in
    [ id
    , rot1
    , rot2
    , mulMats rot1 rot2
    , mulMats rot2 rot2
    ]

tetrahedral : Symmetries
tetrahedral = combineSymms symmXYZ symmYellow

octahedral  : Symmetries
octahedral  = combineSymms tetrahedral [id,(i,negateVec k, j)]

icosahedral : Symmetries
icosahedral = combineSymms tetrahedral symmRed



vecDiv : PhiVec -> PhiVec -> Maybe PhiNum
vecDiv u v =
    let uu = dot u u
        uv = dot u v 
    in if less zero uv 
    && equal 
        (mul uv uv) 
        (mul uu (dot v v))
    then Just <| div uu uv
    else Nothing
