{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module SkolemEngine
  ( -- * Shaders
    createProgram,
    useProgram,

    -- * Assets
    readAsset,

    -- * Utils
    withPtr,
    unlessM,
    radians,

    -- * Buffers
    uploadVector,

    -- * UI
    runUI,
  )
where

import Codec.Wavefront
import Control.Monad
  ( unless,
    when,
  )
import Data.IORef
import Data.Int (Int32)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable
import Foreign (allocaBytes)
import Foreign.C.String
import Foreign.Marshal (alloca)
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW

-- * shader code

data Shader = Shader GLuint [(FilePath, GLuint, IORef String)]

createProgram :: [(GLenum, FilePath)] -> IO Shader
createProgram shaders = do
  prg <- glCreateProgram

  Shader prg
    <$> for
      shaders
      ( \(t, path) -> do
          s <- glCreateShader t
          glAttachShader prg s

          ref <- newIORef ""
          pure (path, s, ref)
      )

setShaderSource :: GLuint -> String -> IO ()
setShaderSource s content = do
  let size = fromIntegral $ length content
  withCString content $ \pstr -> do
    with size $ \psize -> do
      with pstr $ \ppstr -> do
        poke ppstr pstr
        glShaderSource s 1 ppstr psize

useProgram :: Shader -> IO ()
useProgram (Shader prg shaders) = do
  changed <- for shaders $ \(path, s, currentContentRef) -> do
    content <- readFile path

    currentContent <- readIORef currentContentRef

    if content /= currentContent
      then do
        putStrLn $ "Building " <> path
        writeIORef currentContentRef content
        setShaderSource s content
        glCompileShader s

        compileStatus <- withPtr $ glGetShaderiv s GL_COMPILE_STATUS

        if compileStatus /= GL_TRUE
          then do
            putStrLn "Compilation error"
            infoLogSize <- withPtr $ glGetShaderiv s GL_INFO_LOG_LENGTH
            msg <- allocaBytes (fromIntegral infoLogSize) $ \p -> do
              glGetShaderInfoLog s infoLogSize nullPtr p
              peekCString p
            unless (null msg) $ do
              putStrLn msg
            pure False
          else pure True
      else pure False

  when (or changed) $ do
    putStrLn "Linking Program"
    glLinkProgram prg

    linkStatus <- withPtr $ \p -> glGetProgramiv prg GL_LINK_STATUS p

    when (linkStatus /= GL_TRUE) $ do
      putStrLn "Link error"
      infoLogSize <- withPtr $ glGetProgramiv prg GL_INFO_LOG_LENGTH
      msg <- allocaBytes (fromIntegral infoLogSize) $ \p -> do
        glGetProgramInfoLog prg infoLogSize nullPtr p
        peekCString p
      unless (null msg) $ do
        putStrLn msg
    glUseProgram prg

-- * utils

radians :: Floating a => a -> a
radians v = v / 180 * pi

unlessM :: IO Bool -> IO () -> IO ()
unlessM p act = do
  b <- p
  unless b act

withPtr :: Foreign.Storable.Storable a => (Ptr a -> IO ()) -> IO a
withPtr f = alloca $ \p -> do
  f p
  peek p

packLocation :: Vector.Vector Location -> VectorStorable.Vector Float
packLocation v =
  VectorStorable.concat $
    map
      (\(Location x y z _) -> VectorStorable.fromList [x, y, z])
      (Vector.toList v)

packIndexes :: Vector.Vector (Element Face) -> VectorStorable.Vector Int32
packIndexes v =
  VectorStorable.concat $
    map
      ( \(elValue -> Face x y z []) ->
          VectorStorable.fromList (map (fromIntegral . faceLocIndex) [x, y, z])
      )
      (Vector.toList v)

-- * assets

readAsset :: FilePath -> IO (VectorStorable.Vector Float, VectorStorable.Vector Int32)
readAsset path = do
  Right asset <- fromFile path

  let locations = packLocation $ objLocations asset
      indexes :: VectorStorable.Vector Int32 =
        VectorStorable.map (subtract 1) $ packIndexes $ objFaces asset

  pure (locations, indexes)

uploadVector :: forall a. Storable a => GLuint -> VectorStorable.Vector a -> IO ()
uploadVector vbo vec = do
  let size =
        fromIntegral $
          VectorStorable.length vec
            * sizeOf (undefined :: a)
  VectorStorable.unsafeWith vec $
    \d -> glNamedBufferData vbo size (castPtr d) GL_STATIC_DRAW

keyCallback :: GLFW.Window -> GLFW.Key -> p -> GLFW.KeyState -> p' -> IO ()
keyCallback win key _scancode action _mods = do
  when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $ do
    GLFW.setWindowShouldClose win True

runUI :: (GLFW.Window -> (IO () -> IO ()) -> IO ()) -> IO ()
runUI render = do
  True <- GLFW.init

  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 5)

  Just win <- GLFW.createWindow 640 480 "My super application" Nothing Nothing
  GLFW.setKeyCallback win (Just keyCallback)

  GLFW.makeContextCurrent (Just win)

  let loop p = do
        unlessM (GLFW.windowShouldClose win) $ do
          _ <- p
          GLFW.swapBuffers win
          GLFW.pollEvents
          loop p
  render win loop
