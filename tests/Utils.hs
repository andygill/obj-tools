{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
import Linear.V3 (V3(..),cross)
import Linear.Vector
import Linear.Affine
import Linear.Metric
import Linear.Quaternion
import Linear.Matrix as M
import Linear.Epsilon

import qualified Data.List as List
import qualified Data.Text as T

import Debug.Trace as D

import Test.QuickCheck
import Control.Applicative

-- imports from the package under test
import Linear.Quaternion.Utils

main :: IO ()
main = do
     quickCheckWith stdArgs { maxSuccess = 1000000 } prop_betweenq
     putStrLn "Success!"

prop_betweenq (v1::V3 Double) v2 =
    not (nearZero v1 || nearZero v2) ==>
--    not (negate v1 == v2) ==> -- TODO: remote this qualifier
--    label (show (v1,v2)) $ 
    nearZero $ test_betweenq (normalize v1) (normalize v2)

test_betweenq v1 v2 = rotate (betweenq v1 v2) v1 - v2

instance (Num a, Arbitrary a) => Arbitrary (V3 a) where
    arbitrary = V3 <$> a <*> a <*> a
     where a = oneof [pure 0, pure 1, pure (-1), arbitrary]


