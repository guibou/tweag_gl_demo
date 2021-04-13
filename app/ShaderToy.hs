{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wall #-}

module Main where

import GHC.Float
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import SkolemEngine
import System.Environment (getArgs)

main :: IO ()
main = do
  [shaderpath] <- getArgs

  runUI $ \win loop -> do
    prg <- createProgram [(GL_VERTEX_SHADER, "base_toy.vert"), (GL_FRAGMENT_SHADER, shaderpath)]

    -- Nice empty VAO ;)
    vao <- withPtr $ glCreateVertexArrays 1
    glBindVertexArray vao

    glClearColor 1 1 1 1

    loop $ do
      useProgram prg

      Just (double2Float -> t) <- GLFW.getTime
      (w, h) <- GLFW.getFramebufferSize win
      (realToFrac -> mouseX, realToFrac -> mouseY) <- GLFW.getCursorPos win

      glViewport 0 0 (fromIntegral w) (fromIntegral h)
      glClear GL_COLOR_BUFFER_BIT

      glUniform2f 0 (fromIntegral w) (fromIntegral h)
      glUniform1f 1 t
      glUniform2f 2 mouseX (fromIntegral h - mouseY)

      glDrawArrays
        GL_TRIANGLE_STRIP
        0
        4
      pure ()
