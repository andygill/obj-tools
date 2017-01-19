{-# LANGUAGE ScopedTypeVariables #-}
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

import Debug.Trace as D

import Test.QuickCheck
import Control.Applicative

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
               DSL.radiusBottom 0.03
               DSL.radiusTop 0.00
               let d = distanceA v1 v2
               DSL.height (f $ distanceA v1 v2 - 0.05)
               DSL.position (f x,f y,f z)
               let q = rotateBetween (V3 0 (1) 0) (v2 .-. v1)
               let (roll,pitch,yaw) = quaternionToYXZEuler q
               DSL.rotation $ D.trace (show (roll,pitch,yaw)) $ (g roll,g pitch,g yaw)
               DSL.from $ T.pack $ show $ (roll,pitch,yaw)
               return ()
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
        (V3 m31 m32 m33) = id $ fromQuaternion q


     rX = asin(- (max (-1) $ min m23 1))
     rY | abs m23 < 0.99999 = atan2 m13    m33
        | otherwise         = atan2 (-m31) m11
     rZ | abs m23 < 0.99999 = atan2 m21    m22
        | otherwise         = 0
               

-- This will be the reverse of the above usage
rotateBetween :: (RealFloat a, Show a, Epsilon a, Floating a) => V3 a -> V3 a -> Quaternion a
rotateBetween v1 v2 
    | nearZero c && nearZero (d - 1) = 1
    | nearZero c && nearZero (d + 1) && (nearZero (mid - v1) || nearZero (mid - v2)) 
                                     = axisAngle (V3 1 0 0) pi 
    | nearZero c && nearZero (d + 1) = rotateBetween v1 mid * rotateBetween mid v2
    | otherwise  = axisAngle cn ca
  where
    c@(V3 xc yc zc) = cross v1 v2
    d = dot (normalize v1) (normalize v2)
    cn = normalize c
    ca = acos d
    q = axisAngle cn ca
    
    mid = V3 0 1 0
        

prop_rotateBetween (v1::V3 Double) v2 =
    not (nearZero v1 || nearZero v2) ==>
--    not (negate v1 == v2) ==> -- TODO: remote this qualifier
--    label (show (v1,v2)) $ 
    nearZero $ test_rotateBetween (normalize v1) (normalize v2)

test_rotateBetween v1 v2 = rotate (rotateBetween v1 v2) v1 - v2

instance (Num a, Arbitrary a) => Arbitrary (V3 a) where
    arbitrary = V3 <$> a <*> a <*> a
     where a = oneof [pure 0, pure 1, pure (-1), arbitrary]

