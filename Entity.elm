module Entity exposing (Info, ball, strut)

import Phi.Num as P exposing (sub, toFloat, zero, one, phith)
import Phi.Vec exposing (PhiVec,PhiMat,transpose)

import WebGL exposing (Entity, Mesh, Shader)
import Math.Vector3 exposing (Vec3, fromTuple)
import Math.Matrix4 exposing (Mat4, makeTranslate, makeRotate, mul)
import Mesh exposing (Vertex)
import Mesh.Shared exposing (trimap)

phiVecToVec3 : PhiVec -> Vec3
phiVecToVec3 = fromTuple << trimap P.toFloat

phiMatToMat4 : PhiMat -> Mat4
phiMatToMat4 = (\(x,y,z) -> Math.Matrix4.makeBasis x y z) << trimap phiVecToVec3 << transpose

type alias Info =
    { camera : Mat4
    , perspective : Mat4
    , rot : Mat4
    }

ball : Info -> PhiVec -> List Entity
ball {camera, perspective, rot} v =
    Mesh.entity
        vertexShader
        fragmentShader
        Mesh.ball
        { transform = mul rot <| makeTranslate (phiVecToVec3 v)
        , perspective = perspective
        , camera = camera
        , shade = 0.8
        }

strut : Info -> (PhiVec, PhiVec) -> List Entity
strut {camera, perspective, rot} ((a1,a2,a3),(b1,b2,b3)) =
    let v = (sub b1 a1, sub b2 a2, sub b3 a3)
        findFirst : (a -> Maybe x) -> List a -> Maybe x
        findFirst f aa =
            case aa of
                [] -> Nothing
                (a::aa) -> 
                    case f a of
                        Just x -> Just x
                        Nothing -> findFirst f aa
        findFirst2 : (a -> b -> Maybe x) -> List a -> List b -> Maybe x
        findFirst2 f aa bb = 
            case aa of 
                [] -> Nothing
                (a::aa) ->
                    case findFirst (f a) bb of
                        Just x -> Just x
                        Nothing -> findFirst2 f aa bb
    in findFirst2
        (\symm (strutVec, strutMesh) ->
            Phi.Vec.transform symm strutVec 
            |> Phi.Vec.vecDiv v
            |> Maybe.map ((,,) (phiMatToMat4 symm) strutMesh)
            )
        Phi.Vec.icosahedral
        [ ((zero,zero,one) , Mesh.blue)
        , ((P.mul phith phith,zero,one) , Mesh.yellow)
        ]
        |> Maybe.map 
            (\(mat, mesh, factor) ->
                Mesh.entity
                    vertexShader
                    fragmentShader
                    (mesh factor) -- FIXME
                    { transform = mul rot <| mul (makeTranslate (phiVecToVec3 (a1,a2,a3))) <| mat
                    , perspective = perspective
                    , camera = camera
                    , shade = 0.8
                    })
        |> Maybe.withDefault []




type alias Color = Vec3
type alias Uniforms =
    { transform : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }

vertexShader : Shader Vertex Uniforms { vcolor : Color }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 transform;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * transform * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Color }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]
