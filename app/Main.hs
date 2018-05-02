{-# LANGUAGE DataKinds #-}
module Main where

import Gristle.Syntax

vertex
  :: Value (In (Vec 2 Float))
  -> Value (In (Vec 4 Float))
  -> Value (Uniform (Mat 4 4))
  -> Value (Out (Vec 4 Float))
  -> GLSL ()
vertex position _ _ outColor = do
  let (x, y) = decomp $ readFrom position
      pos4   = vec4 x y 0.0 1.0
  outColor .= pos4

fragment
  :: Value (In (Vec 4 Float))
  -> Value (Out (Vec 4 Float))
  -> GLSL ()
fragment inColor outColor = outColor .= readFrom inColor

-- | A demonstration of using Haskell's type system to generate glsl shaders.
--
-- >>> main
-- -------------------- vertex
-- in vec2 a;
-- in vec4 b;
-- uniform mat4 c;
-- out vec4 d;
-- main () {
--   d = vec4(a[0], a[1], 0.0, 1.0);
-- }
-- <BLANKLINE>
-- -------------------- vertex
-- in vec4 a;
-- out vec4 b;
-- main () {
--   b = a;
-- }
-- <BLANKLINE>
main :: IO ()
main = putStr $ unlines [ replicate 20 '-' ++ " vertex"
                        , glsl $ shader vertex
                        , replicate 20 '-' ++ " fragment"
                        , glsl $ shader fragment
                        ]
