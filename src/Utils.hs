module Utils where

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Graphics.UI.GLUT as GLUT hiding (normalize, cross)
import Linear

textureSize :: Float
textureSize = 1

maxCoord :: Float
maxCoord = 50

scale :: Float
scale = 1/maxCoord

sphereQuaternion :: Quaternion Float
sphereQuaternion = axisAngle (V3 1 0 0) (toRadians 0)

sphereX = 0
sphereY = 0
sphereZ = 0

sphereModelMatrix :: M44 Float
sphereModelMatrix = mkTransformation sphereQuaternion (V3 sphereX sphereY sphereZ)

genSphere :: Float -> Float -> Float -> [Float]
genSphere radius verticals horizontals = concat $ do
  -- u <- [2*pi*x/verticals | x <- [0..verticals]]
  -- v <- [pi*x/horizontals | x <- [0..horizontals]]

  m <- [x | x <- [0..(verticals-1)]]
  n <- [x | x <- [0..(horizontals-1)]]

  let u = 2*pi*m/verticals
      v = pi*n/horizontals

  let un = 2*pi*(m+1)/verticals
      vn = pi*(n+1)/horizontals

  let p1 = point u v
  let p2 = point u vn
  let p3 = point un v
  let p4 = point un vn

  return $ concat [p1, p3, p2, p4, p2, p3]
    where
      point u v = let x = radius*(cos u)*(sin v)
                      y = radius*(cos v)
                      z = radius*(sin u)*(sin v)
                      (a,b) = sphereToTexture radius (x,y,z)
                  in (if isNaN a || isNaN b then [x,y,z,maxCoord + 1,maxCoord + 1] else [x,y,z,a,b])
                   --in Vertex3 x y z

sphereToTexture :: Float -> (Float, Float, Float) -> (Float, Float)
sphereToTexture r (x,y,z) = ((r*x/(r-y) + textureSize)/(2*textureSize), (r*z/(r-y) + textureSize)/(2*textureSize))

sphereToPlane :: [Float] -> [Float]
sphereToPlane [x,y,z,u,v] = -- [x/(1-y), 0, z/(1-y), u, v]
  let vec = V4 x y z 1
      (V4 x' y' z' _) = sphereModelMatrix !* vec
  in [(sphereY + 1)*x'/(1-y'), 0, (sphereY + 1)*z'/(1-y'), u, v]

color3f r g b = color $ Color3 r g (b :: GLfloat)
color4f r g b a = color $ Color4 r g b (a :: GLfloat)
vertex3f (x,y,z) = vertex $ Vertex3 x y (z :: GLfloat)

-- normalize :: (Fractional a, Floating a) => Vector3 a -> Vector3 a
-- normalize (Vector3 x y z) = let length = sqrt (x^2 + y^2 + z^2) in Vector3 (x/length) (y/length) (z/length)

-- subVertex :: Num a => Vertex3 a -> Vertex3 a -> Vector3 a
-- subVertex (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)

-- cross :: Num a => Vector3 a -> Vector3 a -> Vector3 a
-- cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2-y1*x2)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a,b) = splitAt n xs
                in a : (chunksOf n b)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = if elem x xs then removeDuplicates xs else x:(removeDuplicates xs)

removeNaNTriangles :: [[Float]] -> [[Float]]
removeNaNTriangles vertices = concat $ filter (not . any hasNaN) $ chunksOf 3 vertices
   where
     hasNaN [x,y,z,u,v] = isNaN x || isNaN y || isNaN z

toRadians :: Floating a => a -> a
toRadians a = a*pi/180
