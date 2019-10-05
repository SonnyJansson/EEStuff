{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.Int
import Data.IORef
import Graphics.UI.GLUT as GLUT hiding (normalize, cross)

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Shaders

data Camera = Camera { camPos  :: Vertex3 GLdouble
                     , camLook :: Vertex3 GLdouble
                     , camUp   :: Vector3 GLdouble
                     }

data DisplayData = DisplayData { camera        :: IORef Camera
                               , sphereProgram :: Program
                               }

main :: IO ()
main = do
  (_program, _args) <- getArgsAndInitialize

  -- Camera

  let cameraPos = (Vertex3 0.5 0.35 ((-0.5) :: GLdouble))
  let cameraLook = (Vertex3 0 0 (0 :: GLdouble))

  let camDirection = normalize (cameraLook `subVertex` cameraPos)
  let worldUp = (Vector3 0 1 (0 :: GLdouble))
  let cameraRight = normalize $ cross worldUp camDirection
  let cameraAngle = pi/2
  let cameraUp = cross camDirection cameraRight

  putStrLn $ show cameraUp

  --let camUp = (Vector3 0 1 (0 :: GLdouble))
  camIO <- newIORef $ Camera {camPos = cameraPos, camLook = cameraLook, camUp = cameraUp}
  camera <- get camIO

 -- Programs 

--  putStrLn $ show $ genSphere 0.5 5 5

  sphereProgram <- makeProgram [ (VertexShader, "shaders/sphere.vert")
                               , (FragmentShader, "shaders/sphere.frag") ]

  -- planeProgram <- makeProgram [ (VertexShader, "shaders/plane.vert")
  --                             , (FragmentShader, "shaders/plane.frac") ]

  -- sphereProgram <- loadShaders [
  --    ShaderInfo VertexShader (FileSource "shaders/sphere.vert"),
  --    ShaderInfo FragmentShader (FileSource "shaders/sphere.frac")]

  let displayData = DisplayData { camera = camIO, sphereProgram = sphereProgram }

  window <- createWindow "Hello, World"
  reshapeCallback $= Just reshape
  displayCallback $= display displayData
  --lookAt (camPos camera) (camLook camera) (camUp camera)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
--  lookAt (Vertex3 0 0 (-1)) (Vertex3 0 0 0) (Vector3 0 1 0)
  viewport $= (Position 0 0, size)
  --loadIdentity
  postRedisplay Nothing

display :: DisplayData -> DisplayCallback
display displayData = do
  --clearColor $= Color4 1 1 1 1
  clear [ ColorBuffer ]
  let camIO = camera displayData
  let sphereProg = sphereProgram displayData
  --let planeProg = planeProgram displayData

  camera <- get camIO

  -- loadIdentity
  -- lookAt (camPos camera) (camLook camera) (camUp camera)

  -- complexGrid
  -- gridAxis
  riemannSphere displayData

  -- flush
  putStrLn "Drawn!"

gridAxis :: IO ()
gridAxis = do
  color3f 0 1 0
  lineWidth $= 3
  renderPrimitive Lines $ mapM_ vertex3f
    [ ((-1), 0, 0), (1, 0, 0)
    , (0, (-1), 0), (0, 1, 0)
    , (0, 0, (-1)), (0, 0, 1) :: (GLfloat, GLfloat, GLfloat) ]
  lineWidth $= 1

gridLines :: GLfloat
gridLines = 8

complexGrid :: IO ()
complexGrid = do
  color3f 0 0 0 
  -- Lines parallel to x-axis
  renderPrimitive Lines $ mapM_ vertex3f $ concat $ fmap pointToLines [(x,x) | x <- (fmap (*(2/gridLines)) [(-gridLines/2)..(gridLines/2)])]
  where
    pointToLines (x,y) = [(x, 0 :: GLfloat, (-1)), (x, 0, 1), ((-1), 0, y), (1, 0, y)]

-- riemannSphere :: IO ()
-- riemannSphere = do
--   vertexAttribCoord3 $= (Just $ AttribLocation 0)
--   -- textureFunction $= Blend
--   -- color4f 0 0 0 0.01
--   renderObject Solid $ GLUT.Sphere' 0.25 50 50


riemannSphere :: DisplayData -> IO ()
riemannSphere displayData = do
  sphere <- genObjectName
  bindVertexArrayObject $= Just sphere

  let vertices = genSphere 0.25 50 50
  --let vertices = [(Vertex3 (-1) 0 0), (Vertex3 0 1 0), (Vertex3 1 0 (0 :: GLfloat))]
      numVertices = (fromIntegral $ length vertices) :: Int32
      vertexSize = (fromIntegral $ sizeOf (head vertices)) :: Int32

  putStrLn $ show numVertices ++ ", " ++ show vertexSize

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let firstIndex = 0 :: Int32
      vPosition = AttribLocation 0

  vertexAttribPointer vPosition $= (ToFloat,
    VertexArrayDescriptor 3 Float (fromIntegral vertexSize) (bufferOffset (firstIndex * vertexSize)))
  vertexAttribArray vPosition $= Enabled

  --bindVertexArrayObject $= Just sphere
  currentProgram $= (Just $ sphereProgram displayData)

  drawArrays Triangles firstIndex numVertices

  flush

  putStrLn "Should have been drawn now"


genSphere :: Float -> Float -> Float -> [Vertex3 GLfloat]
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

  return [p1, p3, p2, p4, p2, p3]
    where
      point u v = let x = radius*(cos u)*(sin v)
                      y = radius*(cos v)
                      z = radius*(sin u)*(sin v)
                   in Vertex3 x y z

color3f r g b = color $ Color3 r g (b :: GLfloat)
color4f r g b a = color $ Color4 r g b (a :: GLfloat)
vertex3f (x,y,z) = vertex $ Vertex3 x y (z :: GLfloat)


normalize :: (Fractional a, Floating a) => Vector3 a -> Vector3 a
normalize (Vector3 x y z) = let length = sqrt (x^2 + y^2 + z^2) in Vector3 (x/length) (y/length) (z/length)

subVertex :: Num a => Vertex3 a -> Vertex3 a -> Vector3 a
subVertex (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)


cross :: Num a => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2-y1*x2)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
