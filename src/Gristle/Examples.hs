-- | Here we list a bunch of examples that will help you get used to writing
-- shaders with "Gristle".
-- Most of these shaders come from [The Book of Shaders](http://thebookofshaders.com).
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Gristle.Examples where

import           Gristle

-- | https://thebookofshaders.com/02/
--
-- >>> putStr $ glsl bos02
-- main () {
--   gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);
-- }
bos02 :: GLSL Fragment ()
bos02 = do
  color <- glFragColor
  shader $ color .= vec4 1 0 1 1


-- | https://thebookofshaders.com/03/
--
-- >>> putStr $ glsl bos03
-- uniform float u_time;
-- main () {
--   gl_FragColor = vec4(abs(sin(u_time)), 0.0, 0.0, 1.0);
-- }
bos03 :: GLSL Fragment ()
bos03 = do
  utime <- named @Uniform @Float "u_time"
  color <- glFragColor
  let r = abs $ sin $ readUniform utime
  shader $ color .= vec4 r 0 0 1


-- | https://thebookofshaders.com/03/
--
-- >>> putStr $ glsl bos03_1
-- uniform vec2 u_resolution;
-- main () {
--   gl_FragColor = vec4((vec2(gl_FragCoord[0], gl_FragCoord[1]) / u_resolution)[0], (vec2(gl_FragCoord[0], gl_FragCoord[1]) / u_resolution)[1], 0.0, 1.0);
-- }
bos03_1 :: GLSL Fragment ()
bos03_1 = do
  urez  <- named "u_resolution"
  color <- glFragColor
  coord <- glFragCoord
  shader $ do
    let (x, y, _, _) = decomp $ readIn coord
        (stx, sty)   = decomp $ vec2 x y ./ readUniform urez
    color .= vec4 stx sty 0 1
