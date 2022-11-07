module LinAlg exposing (..)

import Math.Matrix4 as M4 exposing (..)
import Math.Vector2 exposing (..)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (..)


rotateVector v a =
   let rotation = M4.rotate a (vec3 0 0 1) M4.identity
   in transform rotation v


triangle : List Vec3
triangle =
   let startAngle = (pi/2)
       increment = (2*pi/3)
       initaialVector = vec3 1 0 0
       angles = [startAngle, startAngle + increment, startAngle + 2 * increment]
   in
       List.map (rotateVector initaialVector) angles

makePolygon : Float -> Int -> List Vec3
makePolygon startAngle n =
   let increment = (2*pi/ toFloat n)
       initialVector = vec3 1 0 0
       angles = List.range 0 (n-1) |> List.map ((+) startAngle << (*) increment << toFloat)
   in List.map (rotateVector initialVector) angles

situateShape : Vec3 -> Vec3 -> List Vec3 -> List Vec3
situateShape center scaleVec polygon=
   let translateTrans = M4.translate center M4.identity
       scaleTrans = M4.scale scaleVec M4.identity
   in
       List.map (M4.transform translateTrans << M4.transform scaleTrans) polygon

situatedPolygon = situateShape (vec3 200 200 0) (vec3 80 80 0) <| makePolygon 0 3

parametricPolygon :: Int -> Vec3 -> Vec3 -> Float -> List Vec3
parametricPolygon n radiusVec center startAngle =
   situateShape center radiusVec <| makePolygon startAngle n

aParametricPolygon = parametricPolygon 6 (vec3 80 80 0) (vec3 200 200 0) 0
