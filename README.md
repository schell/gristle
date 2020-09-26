# gristle
This is the spiritial successor to [ixshader](https://github.com/schell/ixshader).
It was started at [BayHac2018](https://wiki.haskell.org/BayHac2018) and I hope to
use it as the bottom layer of rendering in [gelatin](https://github.com/schell/gelatin).

# Table of Contents
1. [Prerequisites](#prerequisites)
2. [Installation](#installation)
3. [Usage](#usage)
4. [Contribuitors](#contribuitors)

# Prerequisites

In order to run this you need to have installed
* Haskell
* Stack
    * [Instructions](https://docs.haskellstack.org/en/stable/README/)
* SDL2
    * [for linux] `sudo apt install libsdl2-dev`

# Installation

* clone this repo
    * `git clone https://github.com/schell/gristle.git`
* enter the directory
    * `cd gristle`
* install the project
    * `ghc Setup.hs`
    * `stack install`

# Usage
`stack install` generates two commands:

    `gristle-exe`

and

    `gristle-example-exe`

* `gristle-exe` generates the glsl code for the haskell code in `gristle/app/Main.hs`

* `gristle-example-exe` runs the code in `gristle-example/app/Main.hs`

## see
```haskell
{-# LANGUAGE DataKinds #-}
module Main where

import Gristle

vertex
  :: Value (In (Vec 2 Float))
  -> Value (In (Vec 4 Float))
  -> Value (Uniform (Mat 4 4))
  -> Value (Out (Vec 4 Float))
  -> GLSL ()
vertex position _ _ outColor = do
  let (x, y) = decomp $ readAttrib position
      pos4   = vec4 x y 0.0 1.0
  outColor .= pos4

fragment :: Value (In (Vec 4 Float)) -> Value (Out (Vec 4 Float)) -> GLSL ()
fragment inColor outColor = outColor .= readAttrib inColor

-- | A demonstration of using Haskell's type system to generate glsl shaders.
--
-- >>> main
-- -------------------- vertex
-- in vec2 a;
-- in vec4 b;
-- uniform mat4 c;
-- out vec4 d;
-- void main () {
--   d = vec4(a[0], a[1], 0.0, 1.0);
-- }
-- <BLANKLINE>
-- -------------------- fragment
-- in vec4 a;
-- out vec4 b;
-- void main () {
--   b = a;
-- }
-- <BLANKLINE>
main :: IO ()
main = putStr $ unlines [ replicate 20 '-' ++ " vertex"
                        , glsl $ shader vertex
                        , replicate 20 '-' ++ " fragment"
                        , glsl $ shader fragment
                        ]
```

# Contribuitors

* [Schell Carl Scivally](https://github.com/schell)
* [Karen Palacio](https://github.com/karen-pal/)
