{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Codec.Wavefront
import Codec.Wavefront.IO (fromFile)
import Control.Monad (unless, when)
import Data.Bits
import Data.Foldable (for_)
import Data.Int (Int32)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable
import Foreign (Ptr, castPtr, nullPtr, peek)
import qualified Foreign as Foreign.Storable
import Foreign.C.String
import Foreign.Marshal (alloca)
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.Float
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import Linear

keyCallback win key _scancode action _mods = do
  when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $ do
    GLFW.setWindowShouldClose win True

main :: IO ()
main = do
  True <- GLFW.init

  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 5)

  Just win <- GLFW.createWindow 640 480 "My super application" Nothing Nothing
  GLFW.setKeyCallback win (Just keyCallback)

  GLFW.makeContextCurrent (Just win)
  -- glDebugMessageCallback errorCallback nullPtr

  Right asset <- fromFile "logo.obj"

  let locations = packLocation $ objLocations asset
      indexes :: VectorStorable.Vector Int32 = VectorStorable.map (subtract 1) $ packIndexes $ objFaces asset

  vao <- withPtr $ glCreateVertexArrays 1
  vboLocations <- withPtr $ glCreateBuffers 1
  vboIndexes <- withPtr $ glCreateBuffers 1

  let size = fromIntegral $ VectorStorable.length locations * sizeOf (undefined :: Float)
  VectorStorable.unsafeWith locations $ \d -> glNamedBufferData vboLocations size (castPtr d) GL_STATIC_DRAW

  let size = fromIntegral $ VectorStorable.length indexes * sizeOf (undefined :: Int32)
  VectorStorable.unsafeWith indexes $ \d -> glNamedBufferData vboIndexes size (castPtr d) GL_STATIC_DRAW

  prg <-
    makeShaderProgram
      [ (GL_VERTEX_SHADER, "base.vert"),
        (GL_FRAGMENT_SHADER, "base.frag")
      ]

  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vboLocations
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER vboIndexes
  {-
  glEnableVertexArrayAttrib vao 0
  glVertexArrayVertexBuffer vao 0 vboLocations 0 (fromIntegral $ sizeOf (undefined :: Float) * 3)
  glVertexArrayAttribFormat vao 0 3 GL_FLOAT GL_FALSE 0
  glVertexArrayAttribBinding vao 0 0

  glVertexArrayElementBuffer vao vboIndexes
  -}

  prg <- attachAndLink [vert, frag]
  glUseProgram prg
  glEnable GL_DEPTH_TEST

  render win indexes

render win indexes = do
  unlessM (GLFW.windowShouldClose win) $ do
    Just (double2Float -> t) <- GLFW.getTime
    (fromIntegral -> w, fromIntegral -> h) <- GLFW.getFramebufferSize win

    let persp = perspective (radians @Float 20) (fromIntegral w / fromIntegral h) 1 1000
        look = lookAt @Float (V3 (50 * cos t) (50 * sin t) 50) (V3 0 0 0) (V3 0 1 0)

        scale = scaled (V4 (0.1 :: Float) 0.1 0.1 1)
        trans = persp !*! look

    with trans $ glUniformMatrix4fv 0 1 GL_TRUE . castPtr

    glViewport 0 0 w h
    glClearColor 1 1 1 1
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    glDrawElements GL_TRIANGLES (fromIntegral $ VectorStorable.length indexes) GL_UNSIGNED_INT nullPtr

    GLFW.swapBuffers win
    GLFW.pollEvents
    render win indexes
  pure ()

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

packLocation v =
  VectorStorable.concat $
    map (\(Location x y z _) -> VectorStorable.fromList [x, y, z]) (Vector.toList v)

packIndexes v =
  VectorStorable.concat $
    map (\(elValue -> Face x y z []) -> VectorStorable.fromList (map (fromIntegral . faceLocIndex) [x, y, z])) (Vector.toList v)

-- * shader code

makeShader t path = do
  content <- readFile path

  s <- glCreateShader t
  let size = fromIntegral $ length content
  withCString content $ \pstr -> do
    with size $ \psize -> do
      with pstr $ \ppstr -> do
        poke ppstr pstr
        glShaderSource s 1 ppstr psize

  glCompileShader s
  pure s

attachAndLink shaders = do
  prg <- glCreateProgram

  for_ shaders $ glAttachShader prg

  glLinkProgram prg
  pure prg

makeShaderProgram l = do
  attachAndLink =<< mapM (uncurry makeShader) l
