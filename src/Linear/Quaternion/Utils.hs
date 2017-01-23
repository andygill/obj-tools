module Linear.Quaternion.Utils where

import Linear.Affine
import Linear.Epsilon
import Linear.Matrix as M
import Linear.Metric
import Linear.Quaternion
import Linear.V3 (V3(..),cross)
import Linear.Vector


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

-- | Convert a 'Quaternion' into a (ordered) 'Euler'.
quaternionToEuler :: RealFloat a => Order -> Quaternion a -> Euler a
quaternionToEuler o@YXZ q = Euler o rX rY rZ
  where
     -- adapted from https://github.com/mrdoob/three.js/blob/master/src/math/Euler.js
     V3 (V3 m11 m12 m13)
        (V3 m21 m22 m23)
        (V3 m31 m32 m33) = id $ fromQuaternion q

     rX = asin(- (max (-1) $ min m23 1))
     rY | abs m23 < 0.99999 = atan2 m13    m33
        | otherwise         = atan2 (-m31) m11
     rZ | abs m23 < 0.99999 = atan2 m21    m22
        | otherwise         = 0
               

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
        
