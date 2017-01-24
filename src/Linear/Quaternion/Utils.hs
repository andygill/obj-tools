module Linear.Quaternion.Utils where

import Linear.Affine
import Linear.Epsilon
import Linear.Matrix as M
import Linear.Metric
import Linear.Quaternion
import Linear.V3 (V3(..),cross)
import Linear.Vector

import qualified Debug.Trace as D

-- | This is an alternative to the Quaternion format,
--   following the THREE.js representation of Euler.
--   These are extrinsic Tait–Bryan rotations (I think)
data Euler a = Euler 
     { order :: Order
     , x :: a
     , y :: a
     , z :: a
     }
     deriving (Eq, Ord, Show, Read)

data Order = XYZ | XZY | YXZ | YZX | ZXY | ZYX
     deriving (Eq, Ord, Show, Read)

reorder :: Order -> V3 a -> V3 a
reorder XYZ (V3 x y z) = V3 x y z
reorder XZY (V3 x y z) = V3 x z y
reorder YXZ (V3 x y z) = V3 y x z
reorder YZX (V3 x y z) = V3 y z x
reorder ZXY (V3 x y z) = V3 z x y
reorder ZYX (V3 x y z) = V3 z y x

invert :: Order -> Order
invert XYZ = XYZ
invert XZY = XZY
invert YXZ = YXZ
invert YZX = ZXY
invert ZXY = YZX
invert ZYX = ZYX
{-
     vX = V3 1 0 0
     vY = V3 0 1 0
     vZ = V3 0 0 1
-}

{-# RULES "invert" forall o . invert (invert o) = o #-}
    
{-# RULES "reorder" forall o v . reorder (invert o) (reorder o v) = v  #-}

-- | Convert a 'Quaternion' into a (ordered) 'Euler'.
quaternionToEuler :: (Epsilon a, RealFloat a) => Order -> Quaternion a -> Euler a
quaternionToEuler o q = Euler o rX rY rZ
  where
     -- adapted from https://github.com/mrdoob/three.js/blob/master/src/math/Euler.js
     V3 (V3 m11 m12 m13)
        (V3 m21 m22 m23)
        (V3 m31 m32 m33) = reorder o $ transpose $ reorder o $ transpose $ fromQuaternion q

     sgn x = case o of
            XZY -> - x
            YXZ -> - x
            ZYX -> - x
            _   ->   x

     V3 rX rY rZ = sgn $ reorder (invert o) $ V3 r1 r2 r3
       where
         gimbal = not $ nearZero $ 1 - abs m13
         r2 = asin $ max (-1) $ min m13 $ 1
         r1 | gimbal    = atan2 (-m23) m33
            | otherwise = atan2   m32  m22
         r3 | gimbal    = atan2 (-m12) m11
            | otherwise = 0
     
{-
    function f(w,x,y,z,order) {
      var q = new THREE.Quaternion(x,y,z,w);
      var e = new THREE.Euler(0,0,0,order);
      e.setFromQuaternion(q);
      return e;
-} 


-- | Take two vectors, and figure a 'Quaternion' that rotates between them.
--   The result 'Quaternion' might not be unique.
betweenq :: (RealFloat a, Show a, Epsilon a, Floating a) => V3 a -> V3 a -> Quaternion a
betweenq v1 v2 
    | nearZero c && nearZero (d - 1) = 1
    | nearZero c && nearZero (d + 1) && (nearZero (mid - v1) || nearZero (mid - v2)) 
                                     = axisAngle (V3 1 0 0) pi 
    | nearZero c && nearZero (d + 1) = betweenq v1 mid * betweenq mid v2
    | otherwise  = axisAngle cn ca
  where
    c@(V3 xc yc zc) = cross v1 v2
    d = dot (normalize v1) (normalize v2)
    cn = normalize c
    ca = acos d
    q = axisAngle cn ca
    
    mid = V3 0 1 0
        
eulerToQuaternion :: (Epsilon a, RealFloat a) => Euler a -> Quaternion a
eulerToQuaternion (Euler o x y z) = 
    case o of
      XYZ -> 1 * _x * _y * _z
      XZY -> 1 * _x * _z * _y
      YXZ -> 1 * _y * _x * _z
      YZX -> 1 * _y * _z * _x
      ZXY -> 1 * _z * _x * _y
      ZYX -> 1 * _z * _y * _x
  where
      _x = axisAngle (V3 1 0 0) x 
      _y = axisAngle (V3 0 1 0) y 
      _z = axisAngle (V3 0 0 1) z


