{-# LANGUAGE OverloadedStrings #-}

module Shaders where

{-
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

-}

  
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Graphics.UI.GLUT

--------------------------------------------------------------------------------

-- | The source of the shader source code.

data ShaderSource =
     ByteStringSource B.ByteString
     -- ^ The shader source code is directly given as a 'B.ByteString'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   | FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO B.ByteString
getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ packUtf8 str
getSource (FileSource path) = B.readFile path

--------------------------------------------------------------------------------

-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo ShaderType ShaderSource
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | Create a new program object from the given shaders, throwing an
-- 'IOException' if something goes wrong.

loadShaders :: [ShaderInfo] -> IO Program
loadShaders infos =
   createProgram `bracketOnError` deleteObjectName $ \program -> do
      putStrLn "Compiling program"
      loadCompileAttach program infos
      linkAndCheck program
      return program

linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadCompileAttach :: Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) =
   createShader shType `bracketOnError` deleteObjectName $ \shader -> do
      src <- getSource source
      shaderSourceBS shader $= src
      compileAndCheck shader
      attachShader program shader
      loadCompileAttach program infos

compileAndCheck :: Shader -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   ok <- get (getStatus object)
   unless ok $ do
      infoLog <- get (getInfoLog object)
      fail (message ++ " log: " ++ infoLog)
