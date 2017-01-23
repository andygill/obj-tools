module Linear.Quaternion.Utils where

import Linear.V3 (V3(..),cross)
import Linear.Vector
import Linear.Affine
import Linear.Metric
import Linear.Quaternion
import Linear.Matrix as M

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

-- adapted from https://github.com/mrdoob/three.js/blob/master/src/math/Euler.js
quaternionToEuler :: RealFloat a => Order -> Quaternion a -> Euler a
quaternionToEuler o@YXZ q = Euler o rX rY rZ
  where
     V3 (V3 m11 m12 m13)
        (V3 m21 m22 m23)
        (V3 m31 m32 m33) = id $ fromQuaternion q

     rX = asin(- (max (-1) $ min m23 1))
     rY | abs m23 < 0.99999 = atan2 m13    m33
        | otherwise         = atan2 (-m31) m11
     rZ | abs m23 < 0.99999 = atan2 m21    m22
        | otherwise         = 0
               
