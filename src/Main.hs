{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.Int
import Data.IORef
import Graphics.UI.GLUT as GLUT hiding (normalize, cross)
import Graphics.GLUtil (readTexture, texture2DWrap, ShaderProgram, simpleShaderProgram, setUniform, program)

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Linear

import Shaders
import Utils

data Camera = Camera { camPos  :: V3 GLfloat
                     , camLook :: V3 GLfloat
                     , camUp   :: V3 GLfloat
                     } deriving (Show)

data ScreenObject = ScreenObject VertexArrayObject ArrayIndex NumArrayIndices

data DisplayData = DisplayData { camera        :: IORef Camera
                               , sphereProgram :: ShaderProgram
                               , planeProgram  :: ShaderProgram
                               , sphere        :: ScreenObject
                               , plane         :: ScreenObject
                               }

imagePathSphere :: FilePath
imagePathSphere = "assets/instagram-grid-border-transparency.png"

imagePathPlane :: FilePath
imagePathPlane = "assets/instagram-grid-partial-transparency.png"

eyePosition = (V3 1 0.35 (1 :: Float))

worldSize :: Float
worldSize = 50

scal = 1/4

sphereResX = 500
sphereResY = 500

sphereScaleMatrix :: M44 Float
sphereScaleMatrix = V4 (V4 scal 0 0 0)
                       (V4 0 scal 0 0)
                       (V4 0 0 scal 0)
                       (V4 0 0 0 1)

planeScaleMatrix :: M44 Float
planeScaleMatrix = V4 (V4 scal 0 0 0)
                      (V4 0 scal 0 0)
                      (V4 0 0 scal 0)
                      (V4 0 0 0 1)

planeModelMatrix :: M44 Float
planeModelMatrix = V4 (V4 1 0 0 0)
                      (V4 0 1 0 0)
                      (V4 0 0 1 0)
                      (V4 0 0 0 1)

main :: IO ()
main = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [ RGBAMode, WithDepthBuffer ]
  initialWindowSize $= Size 512 512
  initialContextVersion $= (4, 3)
  initialContextFlags $= [ DebugContext ]
  initialContextProfile $= [ CoreProfile ]
  window <- createWindow "Hello, World"

  depthFunc $= Just Less
  depthMask $= Enabled

  displayData <- initialise
  camera <- get $ camera displayData

  reshapeCallback $= Just reshape
  displayCallback $= display displayData
  -- lookAt (camPos camera) (camLook camera) (camUp camera)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
--  lookAt (Vertex3 0 0 (-1)) (Vertex3 0 0 0) (Vector3 0 1 0)
  viewport $= (Position 0 0, size)
  --loadIdentity
  postRedisplay Nothing

display :: DisplayData -> DisplayCallback
display displayData = do
  clearColor $= Color4 1 1 1 1
  clear [ ColorBuffer, DepthBuffer ]
  let camIO = camera displayData

  camera <- get camIO

  currentProgram $= Nothing

  -- let viewMatrix = cameraToViewMatrix camera
  -- matrixMode $= Modelview 0
  -- loadIdentity
  -- lookAt (camPos camera) (camLook camera) (camUp camera)

  -- complexGrid
  -- gridAxis
  riemannSphere displayData
  -- complexPlane displayData

  flush
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
  let (ScreenObject s a n) = sphere displayData
  cam <- get $ camera displayData

  blend $= Disabled

  let viewMatrix = cameraToViewMatrix cam
  let projectionMatrix = Linear.perspective (toRadians 45) 1 0.1 100 :: M44 Float

  bindVertexArrayObject $= Just s

  let prog = sphereProgram displayData
  currentProgram $= (Just $ program prog)
  setUniform prog "modelMatrix" sphereModelMatrix
  setUniform prog "scaleMatrix" sphereScaleMatrix
  setUniform prog "viewMatrix" viewMatrix
  setUniform prog "projectionMatrix" projectionMatrix

  drawArrays Triangles a n

  bindVertexArrayObject $= Nothing
  currentProgram $= Nothing

complexPlane :: DisplayData -> IO ()
complexPlane displayData = do
  let (ScreenObject p a n) = plane displayData
  cam <- get $ camera displayData

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  let viewMatrix = cameraToViewMatrix cam
  let projectionMatrix = Linear.perspective (toRadians 45) 1 0.1 100 :: M44 Float

  bindVertexArrayObject $= Just p

  let prog = planeProgram displayData
  currentProgram $= (Just $ program prog)
  setUniform prog "modelMatrix" planeModelMatrix
  setUniform prog "scaleMatrix" planeScaleMatrix
  setUniform prog "viewMatrix" viewMatrix
  setUniform prog "projectionMatrix" projectionMatrix

  drawArrays Triangles a n

  bindVertexArrayObject $= Nothing
  currentProgram $= Nothing

initialise :: IO DisplayData
initialise =  do
  -- Camera

  let cameraPos = eyePosition
  let cameraLook = (V3 0 0 (0 :: Float))

  let camDirection = normalize (cameraLook - cameraPos)
  let worldUp = (V3 0 1 (0 :: Float))
  let cameraRight = normalize $ cross worldUp camDirection
  let cameraAngle = pi/2
  let cameraUp = cross camDirection cameraRight

  putStrLn $ show cameraUp

  --let camUp = (Vector3 0 1 (0 :: GLdouble))
  camIO <- newIORef $ Camera {camPos = cameraPos, camLook = cameraLook, camUp = cameraUp}
  camera <- get camIO

  putStrLn $ show camera

  -- Loading programs

  -- sphereProgram <- loadShaders [
  --     ShaderInfo VertexShader (FileSource "shaders/sphere.vert"),
  --     ShaderInfo FragmentShader (FileSource "shaders/sphere.frag")] 

  -- planeProgram <- loadShaders [
  --     ShaderInfo VertexShader (FileSource "shaders/plane.vert"),
  --     ShaderInfo FragmentShader (FileSource "shaders/plane.frag")] 

  sphereProgram <- simpleShaderProgram "shaders/sphere.vert" "shaders/sphere.frag"
  planeProgram <- simpleShaderProgram "shaders/plane.vert" "shaders/plane.frag"

  putStrLn "Programs finished"

  -- Objects

  sphereObject <- initialiseSphere
  planeObject <- initialisePlane

  -- Initialise texture

  currentProgram $= Just (program sphereProgram)

  activeTexture $= TextureUnit 0
  let tex_00 = imagePathSphere
  tx0 <- loadTex tex_00
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just tx0

  setUniform sphereProgram "tex_00" (TextureUnit 0)

  currentProgram $= Just (program planeProgram)

  activeTexture $= TextureUnit 1
  let tex_01 = imagePathPlane
  tx1 <- loadTex tex_01
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just tx1

  setUniform planeProgram "tex_00" (TextureUnit 1)

  currentProgram $= Nothing

  -- Put together displaydata

  let displayData = DisplayData { camera = camIO, sphereProgram = sphereProgram, planeProgram = planeProgram, sphere = sphereObject, plane = planeObject }

  return displayData


-- Sphere object initialisation
initialiseSphere :: IO ScreenObject
initialiseSphere = do
  sphere <- genObjectName
  bindVertexArrayObject $= Just sphere

  let floatSize = (fromIntegral $ sizeOf (0.0 :: GLfloat)) :: GLsizei
      stride = 5*floatSize

  let vertices = genSphere 1 sphereResX sphereResY
  --let vertices = [(Vertex3 (-1) 0 0), (Vertex3 0 1 0), (Vertex3 1 0 (0 :: GLfloat))]
      numVertices = div (fromIntegral $ length vertices) 5 :: Int32
      vertexSize = 5*(fromIntegral $ sizeOf (head vertices)) :: Int32

  -- putStrLn $ show $ chunksOf 5 vertices
  putStrLn $ show $ maximum vertices

  putStrLn $ show numVertices ++ ", " ++ show vertexSize

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  putStrLn "Hello"
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    putStrLn "Hello2"
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  putStrLn "Hello3"

  -- Positions
  
  let firstIndex = 0 :: Int32
      posOffset = 0 * floatSize :: Int32
      vPosition = AttribLocation 0

  vertexAttribPointer vPosition $= (ToFloat,
    VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
  vertexAttribArray vPosition $= Enabled

  -- UV coords

  let uvOffset = 3 * floatSize :: Int32
      uvCoords = AttribLocation 1

  vertexAttribPointer uvCoords $= (ToFloat,
    VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
  vertexAttribArray uvCoords $= Enabled

  bindVertexArrayObject $= Nothing
  bindBuffer ArrayBuffer $= Nothing

  return $ ScreenObject sphere firstIndex numVertices

initialisePlane :: IO ScreenObject
initialisePlane = do
  plane <- genObjectName
  bindVertexArrayObject $= Just plane

  let floatSize = (fromIntegral $ sizeOf (0.0 :: GLfloat)) :: GLsizei
      stride = 5*floatSize

  let vertices = concat $ removeNaNTriangles $ fmap sphereToPlane $ chunksOf 5 $ genSphere 1 sphereResX sphereResY
  --let vertices = [(Vertex3 (-1) 0 0), (Vertex3 0 1 0), (Vertex3 1 0 (0 :: GLfloat))]
      numVertices = div (fromIntegral $ length vertices) 5 :: Int32
      vertexSize = 5*(fromIntegral $ sizeOf (head vertices)) :: Int32

  -- putStrLn $ show $ chunksOf 5 vertices
  putStrLn $ show $ maximum vertices

  putStrLn $ show numVertices ++ ", " ++ show vertexSize

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  putStrLn "Hello"
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    putStrLn "Hello2"
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  putStrLn "Hello3"

  -- Positions

  let firstIndex = 0 :: Int32
      posOffset = 0 * floatSize :: Int32
      vPosition = AttribLocation 0

  vertexAttribPointer vPosition $= (ToFloat,
    VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
  vertexAttribArray vPosition $= Enabled

  -- UV coords

  let uvOffset = 3 * floatSize :: Int32
      uvCoords = AttribLocation 1

  vertexAttribPointer uvCoords $= (ToFloat,
    VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
  vertexAttribArray uvCoords $= Enabled

  bindVertexArrayObject $= Nothing
  bindBuffer ArrayBuffer $= Nothing

  return $ ScreenObject plane firstIndex numVertices

loadTex :: FilePath -> IO TextureObject
loadTex f = do
  t <- either error id <$> readTexture f
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  texture2DWrap $= (Repeated, ClampToEdge)
  return t

cameraToViewMatrix :: Camera -> (M44 GLfloat)
cameraToViewMatrix camera = Linear.lookAt (camPos camera) (camLook camera) (camUp camera)
