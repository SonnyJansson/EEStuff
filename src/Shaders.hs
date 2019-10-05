{-# LANGUAGE OverloadedStrings #-}

module Shaders where

import Control.Monad
import qualified Data.ByteString as BS
import Graphics.UI.GLUT as GLUT hiding (normalize, cross)

shaderFromSource :: ShaderType -> FilePath -> IO Shader
shaderFromSource shaderType path = do
  shader <- createShader shaderType
  source <- BS.readFile path
  BS.putStrLn source
  shaderSourceBS shader $= source
  compileShader shader
  infoLog <- shaderInfoLog shader
  putStrLn ("Shader infoLog: " ++ infoLog)
  return shader

makeProgram :: [(ShaderType, FilePath)] -> IO Program
makeProgram shadersInfo = do
  program <- createProgram
  shaders <- mapM (uncurry shaderFromSource) shadersInfo
  mapM_ (attachShader program) shaders
  linkProgram program
  validateProgram program
  infoLog <- programInfoLog program
  putStrLn ("Program infoLog: " ++ infoLog)
  return program
