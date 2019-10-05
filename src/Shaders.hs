module Shaders where

import qualified Data.ByteString as BS
import Graphics.UI.GLUT as GLUT hiding (normalize, cross)

shaderFromSource :: ShaderType -> BS.ByteString -> IO Shader
shaderFromSource shaderType path = do
  shader <- createShader shaderType
  shaderSourceBS shader $= path
  compileShader shader
  return shader

makeProgram :: [(ShaderType, BS.ByteString)] -> IO Program
makeProgram shadersInfo = do
  program <- createProgram
  shaders <- mapM (uncurry shaderFromSource) shadersInfo
  mapM_ (attachShader program) shaders
  linkProgram program
  return program
