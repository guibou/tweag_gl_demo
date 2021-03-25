{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Bits
import qualified Data.Vector.Storable as VectorStorable
import Foreign
  ( castPtr,
    nullPtr,
  )
import Foreign.Marshal.Utils
import GHC.Float
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import Linear
import SkolemEngine
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs

  runUI $ \win loop -> do
    (locations, indexes) <- readAsset path

    vboLocations <- withPtr $ glCreateBuffers 1
    vboIndexes <- withPtr $ glCreateBuffers 1

    uploadVector vboLocations locations
    uploadVector vboIndexes indexes

    prg <- createProgram [(GL_VERTEX_SHADER, "base.vert"), (GL_FRAGMENT_SHADER, "base.frag")]

    vao <- withPtr $ glCreateVertexArrays 1
    glBindVertexArray vao
    glBindBuffer GL_ARRAY_BUFFER vboLocations
    glEnableVertexAttribArray 0
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER vboIndexes

    glBindVertexArray vao
    glEnable GL_DEPTH_TEST
    glClearColor 1 1 1 1

    loop $ do
      useProgram prg

      Just (double2Float -> t) <- GLFW.getTime
      (fromIntegral -> w, fromIntegral -> h) <- GLFW.getFramebufferSize win

      let persp = perspective (radians @Float 20) (fromIntegral w / fromIntegral h) 1 1000
          look = lookAt @Float (V3 (50 * cos t) (50 * sin t) 50) (V3 0 0 0) (V3 0 1 0)

          trans = persp !*! look

      with trans $ glUniformMatrix4fv 0 1 GL_TRUE . castPtr

      -- update light
      with (V3 (2 * cos (2 * t)) (2 * sin (2 * t)) 2) $ glUniform3fv 1 1 . castPtr

      glViewport 0 0 w h
      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

      glDrawElements
        GL_TRIANGLES
        (fromIntegral $ VectorStorable.length indexes)
        GL_UNSIGNED_INT
        nullPtr
      pure ()
