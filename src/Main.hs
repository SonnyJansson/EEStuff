module Main where

import Data.IORef
import Graphics.UI.GLUT as GLUT

data Camera = Camera { camPos  :: Vertex3 GLdouble
                     , camLook :: Vertex3 GLdouble
                     , camUp   :: Vector3 GLdouble
                     }

main :: IO ()
main = do
  (_program, _args) <- getArgsAndInitialize
  camIO <- newIORef $ Camera {camPos = Vertex3 0.5 0.5 (0.5 :: GLdouble), camLook = Vertex3 0 0 (0 :: GLdouble), camUp = Vector3 0 1 (0 :: GLdouble)}
  window <- createWindow "Hello, World"
  reshapeCallback $= Just reshape
  displayCallback $= display camIO
  mainLoop


reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: IORef Camera -> DisplayCallback
display camIO = do
  camera <- get camIO

  clearColor $= Color4 1 1 1 1
  clear [ ColorBuffer ]

  lookAt (camPos camera) (camLook camera) (camUp camera)

  gridAxis
  riemannSphere

  flush
  putStrLn "Drawn!"


gridAxis :: IO ()
gridAxis = do
  color3f 0 1 0
  renderPrimitive Lines $ mapM_ vertex3f
    [ (0, 0, 0), (1, 0, 0)
    , (0, 0, 0), (0, 1, 0)
    , (0, 0, 0), (0, 0, 1) :: (GLfloat, GLfloat, GLfloat) ]

riemannSphere :: IO ()
riemannSphere = do
  textureFunction $= Blend
  color4f 0 0 0 0.01
  renderObject Solid $ GLUT.Sphere' 0.5 50 50

color3f r g b = color $ Color3 r g (b :: GLfloat)
color4f r g b a = color $ Color4 r g b (a :: GLfloat)
vertex3f (x,y,z) = vertex $ Vertex3 x y (z :: GLfloat)
