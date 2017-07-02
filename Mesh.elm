module Mesh exposing (Vertex,entity,ball,blue,yellow)
import WebGL exposing (triangles,lines)
import Math.Vector3 exposing (Vec3, vec3, fromTuple)

import Phi.Num as P
import Phi.Vec exposing (PhiVec)

import Mesh.Shared exposing (bimap,trimap)

import Mesh.Ball
import Mesh.Strut.Blue
import Mesh.Strut.Yellow

type alias Vertex = {position : Vec3, color : Vec3}


trisToMesh : Vec3 -> List (PhiVec,PhiVec,PhiVec) -> WebGL.Mesh Vertex
trisToMesh c =
    List.map (trimap 
        (trimap P.toFloat 
            >> \x -> 
                { position = fromTuple x
                , color = c}))
    >> triangles

edgesToMesh : List (PhiVec,PhiVec) -> WebGL.Mesh Vertex
edgesToMesh =
    List.map (bimap 
        (trimap P.toFloat 
            >> \x -> 
                { position = fromTuple x
                , color = vec3 0 0 0}))
    >> lines

toMesh : Vec3 -> Mesh.Shared.Mesh -> List (WebGL.Mesh Vertex)
toMesh c (tris,edges) =
    [trisToMesh c tris, edgesToMesh edges]


ball = toMesh (vec3 0.9 0.9 0.9) Mesh.Ball.ball
blue l = toMesh (vec3 0 0.4 0.7) (Mesh.Strut.Blue.blue l)
yellow l = toMesh (vec3 0.8 0.7 0) (Mesh.Strut.Yellow.yellow l)

entity
    :  WebGL.Shader attributes uniforms varyings
    -> WebGL.Shader {  } uniforms varyings
    -> List (WebGL.Mesh attributes)
    -> uniforms
    -> List WebGL.Entity
entity vs fs ms u =
    List.map (\m -> WebGL.entity vs fs m u)
    ms

