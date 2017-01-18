module Main where

import Codec.Wavefront
import Linear.V3 (V3(..),cross)
import Linear.Vector
import Linear.Affine
import Linear.Metric
import Linear.Quaternion
import Linear.Matrix as M


import Data.Vector (toList,(!))
import qualified Text.AFrame.DSL as DSL
import Text.AFrame.WebPage
import Text.AFrame (AFrame)
import qualified Data.List as List
import Linear.Epsilon
import qualified Data.Text as T


main = do
        Right obj <- fromFile "example.obj"
        print obj
        let fn :: FaceIndex -> V3 Float
            fn (FaceIndex n _ _) = case objLocations obj ! (n-1) of
                Location x y z _ -> V3 x y z
        let faces = 
                [ F (case elValue f of
                          Face x y z zs -> map fn (x:y:z:zs)
                    ) i
                | (i,f) <- [1..] `zip` toList (objFaces obj) 
                ]
--        let pM = perspective 45 1.0 0

        webPage ["obj1.html"] $ debugFs faces

        print faces

-- Our basic face
data F a = F [V3 Float] a
        deriving Show



debugFs :: [F a] -> AFrame
debugFs fs = DSL.scene $ do
    sequence_
        [ DSL.sphere $ do
               DSL.color "blue"
               DSL.radius 0.02
               DSL.position (f x,f y,f z)
        | V3 x y z <- points
        ]
    sequence_
        [ DSL.cone $ do
               DSL.color $ if i == (1 :: Int) then "red" else "green"
               DSL.radiusBottom 0.00
               DSL.radiusTop 0.03
               let d = distanceA v1 v2
               DSL.height (f $ distanceA v1 v2 - 0.05)
               DSL.position (f x,f y,f z)
               let c@(V3 xc yc zc) = cross (v1 .-. v2) (V3 0 1 0)
               let d = dot (normalize (v1 .-. v2)) (normalize (V3 0 1 0))
               let cn | nearZero c = V3 0 0 1
                      | otherwise  = normalize c
               let ca = acos d
               let V3 x' y' z' = v1 .-. v2
--               let q = axisAngle cn ca
               let q = rotateBetween (V3 0 1 0) (v2 .-. v1)
               let (roll,pitch,yaw) = quaternionToYXZEuler q
               DSL.rotation $ (g roll,g pitch,g yaw)
--               DSL.from $ T.pack $ show $ (q,d,cn,ca)
        | (i,v1,v2) <- edges 
        , let V3 x y z = lerp 0.5 v1 v2        
        ]


 where 
    -- Add to DSL
    f :: Float -> DSL.Number
    f = fromRational . toRational         

    g = f . (/pi) . (* 180)

    edges :: [(Int,V3 Float,V3 Float)]
    edges = -- List.nub $
        [ (i,v,v')
        | F ps _ <- fs 
        , ((v,v'),i) <- ((last ps : init ps) `zip` ps) `zip` [1..]
        ]


    points :: [V3 Float]
    points = List.nub $
        [ v
        | F ps _ <- fs 
        , v <- ps
        ]
                
        

-- AFrame uses YXZ Euler, because headsets use YXZ Euler.
-- adapted from https://github.com/mrdoob/three.js/blob/master/src/math/Euler.js
quaternionToYXZEuler :: (RealFloat a) => Quaternion a -> (a,a,a)
quaternionToYXZEuler q = (rX,rY,rZ)
  where
     V3 (V3 m11 m12 m13)
        (V3 m21 m22 m23)
        (V3 m31 m32 m33) = M.transpose $ fromQuaternion q


     rX = asin(- (max (-1) $ min m23 1))
     rY | abs m23 < 0.99999 = atan2 m13    m33
        | otherwise         = atan2 (-m31) m11
     rZ | abs m23 < 0.99999 = atan2 m21    m22
        | otherwise         = 0
               

-- This will be the reverse of the above usage
rotateBetween :: (Epsilon a, Floating a) => V3 a -> V3 a -> Quaternion a
rotateBetween v1 v2 = q
  where
    c@(V3 xc yc zc) = cross v1 v2
    d = dot (normalize v1) (normalize v2)
    cn | nearZero c = V3 0 0 1 -- TODO: this should be perpendicular to v1
       | otherwise  = normalize c
    ca = acos d
    q = axisAngle cn ca
