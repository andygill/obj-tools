import Codec.Picture
import Debug.Trace
import Data.List
import qualified Data.Map as Map

main = do
  Right img0 <- readImage "PNG_transparency_demonstration_1.png"
  let img = convertRGBA8 img0
--  Right (ImageRGB8 img) <- readImage "PNG_transparency_demonstration_1.png"
  print (imageWidth img)
  print (imageHeight img)
--  print (imageWidth img)

  let imageWidth' :: Double 
      imageWidth' = (1 - 0.00001) * fromIntegral (imageWidth img)
  let imageHeight' :: Double
      imageHeight' = (1 - 0.00001) * fromIntegral (imageHeight img)

  let toPixCoord :: Coord Double -> Coord Int
      toPixCoord (x,y) = (floor $ x * imageWidth', floor $ (1 - y) * imageHeight')
    


  print $ pixelAt img 100 100

  print $ toPixCoord $ (0,0)
  print $ toPixCoord $ (1,1)

  let toPixValue :: Coord Int -> Bool
      toPixValue (x,y) = case pixelAt img x y of
                           PixelRGBA8 _ _ _ a -> a > 100
  
  putStr $ unlines [  [ case pixelAt img x y of
                          PixelRGBA8 _ _ _ a -> if a > 200 then '#' else ' '
                      | x <- map (*10) [0..59]
                      ]
                   | y <- map (*20) [0..39]
                   ]
                   
  let get x = getTess x []

  let t1 = generateMesh 1 toPixCoord toPixValue (Tri (0,0) (1,1) (0,1))  
  let t2 = generateMesh 1 toPixCoord toPixValue (Tri (1,1) (0,0) (1,0))
  let mesh = get $ Split t1 t2

  print $ length $ mesh

  let points = map head $ group $ sort $ concat [ [p1,p2,p3] | Tri p1 p2 p3 <- mesh ]
  
  let pointsDB = Map.fromList (points `zip` [1..])
--  print pointsDB

  let f p = case Map.lookup p pointsDB of
              Just n -> n
              Nothing -> error "internal error"
              
  let g p = show (f p) ++ "/" ++ show (f p) ++ "/1" 

  writeFile "dice.obj" $ unlines $
     ["# geometric vertices"] ++
     ["v " ++ show x ++ " " ++ show y ++ " 0" | (x,y) <- Map.keys pointsDB ] ++
     ["# texture vertices"] ++
     ["vt " ++ show x ++ " " ++ show y ++ " 0" | (x,y) <- Map.keys pointsDB ] ++
     ["# normal"] ++
     ["vn 0 0 1"] ++
     ["# faces"] ++
     ["usemtl image"] ++
     ["f " ++ (g p1) ++ " " ++ (g p2) ++ " " ++ (g p3)
     | (Tri p1 p2 p3) <- mesh 
     ]

type Coord d = (d,d)

type Bounds = (Coord Double,Coord Double)

data Dir = NE | SE | SW | NW
  deriving Show

{-
  data Quad = Quad       Quad Quad Quad Quad
          | Triangle   Bounds Dir
          | Quadrangle Bounds
          | Space
  deriving Show
-}
ep :: Double
ep = 0.01

toPixelCoord :: (Int,Int) -> Dir -> Coord Double -> Coord Int
toPixelCoord (w,h) dir (x,y) = (floor (x*fromIntegral w + xep),floor (y*fromIntegral h + yep))
    where
        (xep,yep) = case dir of
                      NE -> (-ep,ep)
                      SE -> (-ep,-ep)
                      NW -> (ep,ep)
                      SW -> (ep,-ep)
                      

{-

0  1/4 1/2 3/4  1
+---+---+---+---+
| 0 | 1 | 2 | 3 |
+---+---+---+---+

-}
 {- 
generateQuad :: Bounds -> (Dir -> Coord Double -> Coord Int) -> (Coord Int -> Bool) -> Quad
generateQuad b@((x,y),(x',y')) pc f  
  | xy == xy' = if f xy
                then Quadrangle b
                else Space
  | otherwise = _
 where
   xy   = pc NW (x,y)
   xy'  = pc SE (x',y')
   xm   = (x + x') / 2
   ym   = (y + y') / 2
{-
 
   (x,y)      (xm,y)       (x',y)
          Q1           Q2
   (x,ym)     (xm,ym)      (x',ym)
          Q3           Q4
   (x,y')     (xm,y')      (x',y')
-}

   q1   = generateQuad ((x,y),(xm,ym))   pc f
   q2   = generateQuad ((xm,y),(x',ym))  pc f
   q3   = generateQuad ((x,ym),(xm,y')   pc f
   q4   = generateQuad ((xm,ym),(x',y')) pc f

-}


{-
  | otherwise =
    where
        xm = 
        ym = 
-}        


-- The first edge is *always* the longest one.
-- The points go in clockwise direction.
-- The triangles are all right angle triangles.

data Tri = Tri (Coord Double) (Coord Double) (Coord Double)
 deriving Show


data Mesh = Split Mesh Mesh
          | Triangle Tri
          | Empty
    deriving Show

getTess :: Mesh -> [Tri] -> [Tri]
getTess (Split m1 m2) r = getTess m1 (getTess m2 r)
getTess (Triangle t)  r = t : r
getTess Empty         r = r


-- If you return Triangle, then it must be for the incomming Tri
generateMesh :: Int -> (Coord Double -> Coord Int) -> (Coord Int -> Bool) -> Tri -> Mesh
generateMesh n pc f tri@(Tri p1 p2 p3)
--  | traceShow ("genMesh",n,areaOfTriangle tri,pp1 == pp2 && pp2 == pp3 ) False = undefined
  | areaOfTriangle tri < (0.001 ^ 2)
          = if f pp1 && f pp2 && f pp3 -- all three point need to be inside
            then Triangle tri
            else Empty
  | otherwise = case (m1,m2) of
                  (Empty,Empty)           -> Empty
                  (Triangle _,Triangle _) -> Triangle tri  -- both sides are filled
                  _                       -> Split m1 m2
 where
   pp1 = pc p1
   pp2 = pc p2
   pp3 = pc p3
   (t1,t2) = splitTri tri
   m1 = generateMesh (n+1) pc f t1
   m2 = generateMesh (n+1) pc f t2


{-

(x2,y2)
   |   \
   |    \
   |     \
   |      (x3,y3)
   |     /
   |    /
   |   /

(x1,y1) 


(xM,yM) = midCoord (x1,y1) (x2,y2)

(x2,y2)
   |   \
   |    \
   |     \
(xM,yM)-- (x3,y3)

(xM,yM)-- (x3,y3)
   |     /
   |    /
   |   /

(x1,y1) 
-}

splitTri :: Tri -> (Tri,Tri)
splitTri (Tri c1@(x1,y1) c2@(x2,y2) c3) =
        ( Tri c2 c3 cM
        , Tri c3 c1 cM
        )
  where cM = ((x1 + x2) / 2, (y1 + y2) / 2)


p1, p2, p3 :: Coord Double
p1 = (20,20)
p2 = (0,20)
p3 = (10,10)
{-
generateQuad :: Tri -> (Coord Double -> Coord Int) -> (Coord Int -> Bool) -> Quad
generateQuad tri@(Tri p1 p2 p3) pc f  
  | pp1 == pp2 && pp2 == pp3 
          = if f xy
            then tri
            else Space
  | otherwise = _
 where
   pp1 = pc p1
   pp2 = pc p2
   pp3 = pc p3
   xy'  = pc SE (x',y')
   xm   = (x + x') / 2
   ym   = (y + y') / 2
-}


areaOfTriangle :: Tri -> Double
areaOfTriangle (Tri (x1,y1) (x2,y2) _) = (((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)) / 2

