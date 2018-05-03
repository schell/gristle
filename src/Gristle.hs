module Gristle
  ( -- * Values and types
    Value
  , false
  , true
  , Vec
  , vec2
  , vec3
  , vec4
  , Mat
  , mat2
  , mat3
  , mat4
    -- * Multiplying vectors and matrices
  , mult
    -- * Decomposing vectors and matrices
  , ComponentsOf (..)
    -- * Array access
  , atIndex
    -- * Uniform and In (uniform, in, attribute)
  , Uniform
  , In
  , readFrom
    -- * Out (out, varying)
  , Out
    -- * The GLSL monad
  , GLSL
  , glsl
    -- * Writing shaders
  , shader
  , (.=)
  ) where

import Gristle.Syntax
import Gristle.Mult
