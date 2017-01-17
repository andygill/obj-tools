module Main where

import Codec.Wavefront
import Linear.V3 (V3(..))
import Linear.Vector
import Linear.Affine


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
               DSL.color "green"
               DSL.radiusBottom 0.03
               DSL.radiusTop 0.00
               let d = distanceA v1 v2
               DSL.height (f $ distanceA v1 v2 - 0.05)
               DSL.position (f x,f y,f z)
               let V3 x' y' z' = v1 .-. v2
               let xy = if nearZero y' then pi/2 else atan (x' / y')
               let xz = if nearZero z' then pi/2 else atan (x' / z')
               DSL.from $ T.pack $ show $ (x',y',xy,xz)
               DSL.rotation (0, f $ 90 + xz * (180 / pi), f $ xy * (180 / pi) )
        | (v1,v2) <- edges
        , let V3 x y z = lerp 0.5 v1 v2        
        ]


 where 
    -- Add to DSL
    f :: Float -> DSL.Number
    f = fromRational . toRational         

    edges :: [(V3 Float,V3 Float)]
    edges = -- List.nub $
        [ (v,v')
        | F ps _ <- fs 
        , (v,v') <- ps `zip` (tail ps ++ [ head ps])
        ]


    points :: [V3 Float]
    points = List.nub $
        [ v
        | F ps _ <- fs 
        , v <- ps
        ]
                
        