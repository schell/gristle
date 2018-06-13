module Gristle
  ( -- * Values and types
    Value
  , NamedValue
  , false
  , true
  -- * Vectors
  , Vec
  , vec2
  , vec3
  , vec4
  , ivec2
  , ivec3
  , ivec4
  , uvec2
  , uvec3
  , uvec4
  , bvec2
  , bvec3
  , bvec4
  , Mat
  , mat2
  , mat3
  , mat4
    -- * Operations on like vectors
  , (./)
  , (.+)
  , (.-)
    -- * Multiplying vectors with matrices
  , mult
    -- * Decomposing vectors and matrices
  , ComponentsOf (..)
    -- * Array access
  , atIndex
    -- * Uniforms
  , Uniform
  , uniform
  , readUniform
    -- * Attributes
  , In
  , attribute
  , readIn
    -- * Reading Uniform or In
  , readFrom
    -- * Out
  , Out
  , out
  , (.=)
  , set
    -- * Explicit naming
  , named
    -- * Special linkages
  , glPosition
  , glFragColor
  , glFragCoord
    -- * The GLSL monad
  , GLSL
  , ShaderContext(..)
  , Vertex
  , Fragment
  , glsl
  , (:&)(..)
    -- * Writing shaders
  , Shader
  , shader
  , var
  , ifthen
  , ifthenelse
  , switch
  , Gristle.GLSL.break
  , discard
    -- * Linking shader inputs and outputs
  , Link (..)
  , linkAs
  , linkages
  , ShaderLinkages (..)
  ) where


import           Gristle.GLSL
import           Gristle.Linkage
import           Gristle.Mult
import           Gristle.Syntax
import           Gristle.Types            ()
import           Gristle.Vector
