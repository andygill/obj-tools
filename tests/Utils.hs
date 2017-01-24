{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GeneralizedNewtypeDeriving, TypeFamilies #-}
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
     quickCheckWith stdArgs { maxSuccess = 1000000 } prop_q2e2q
     putStrLn "Success!"

------------------------------------------------------------------------------
prop_betweenq (A'V3 v1) (A'V3 v2) =
    not (nearZero v1 || nearZero v2) ==>
    nearZero $ test_betweenq (normalize v1) (normalize v2)

test_betweenq v1 v2 = rotate (betweenq v1 v2) v1 - v2

------------------------------------------------------------------------------

prop_q2e2q (A'Quaternion q) = eqQ (test_q2e2q q) q
test_q2e2q = eulerToQuaternion . quaternionToEuler YXZ

eqQ :: (Epsilon a, RealFloat a) => Quaternion a -> Quaternion a -> Bool
eqQ qa qb = nearZero (qa - qb) || nearZero (qa + qb)

------------------------------------------------------------------------------
-- Momomophic for testing
newtype A'V3 = A'V3 (V3 Double)
  deriving Show

instance Arbitrary A'V3 where
    arbitrary = A'V3 <$> (V3 <$> a <*> a <*> a)
     where a = oneof [pure 0, pure 1, pure (-1), arbitrary]


-- Momomophic for testing, only generates unit 'Quaternion's.
newtype A'Quaternion = A'Quaternion (Quaternion Double)
  deriving Show

instance Arbitrary A'Quaternion where
    arbitrary = do
    	      A'V3 v <- arbitrary
	      if nearZero v
	      then arbitrary -- re-try
	      else do r <- oneof $ arbitrary 
	      	      	         : [pure (n*m) 
				   | n <- [-1,1]
				   , m <- [0,pi/4,pi/3,pi/2,pi]
				   ]
	      	      return $ A'Quaternion $ axisAngle v r

