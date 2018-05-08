{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | This module provides matrix and vector multiplication.
module Gristle.Mult where

import           Data.Type.Equality

import           Gristle.Syntax
import           Gristle.Vector

-- $setup
-- >>> :set -XTypeApplications -XDataKinds -XTypeFamilies -XFlexibleContexts
-- >>> :set -XAllowAmbiguousTypes
-- >>> import GHC.TypeLits (Nat)
-- >>> import Gristle.Linkage
-- >>> import Gristle.GLSL


-- | Describes the number of columns and rows in a type, at the type level.
--
-- >>> :kind! ColumnsAndRows (Vec 4 Int) Int
-- ColumnsAndRows (Vec 4 Int) Int :: (Nat, Nat)
-- = '(4, 1)
--
-- >>> :kind! ColumnsAndRows Int Int
-- ColumnsAndRows Int Int :: (Nat, Nat)
-- = '(1, 1)
--
-- >>> :kind! ColumnsAndRows (Vec 3 Float) (Mat 2 3)
-- ColumnsAndRows (Vec 3 Float) (Mat 2 3) :: (Nat, Nat)
-- = '(2, 1)
type family ColumnsAndRows x y where
  ColumnsAndRows (Vec c (Vec m t)) (Vec n (Vec c t)) = '(n, m)
  ColumnsAndRows (Vec c t)         (Vec n (Vec c t)) = '(n, 1)
  ColumnsAndRows (Vec c (Vec m t)) (Vec c t)         = '(1, m)
  ColumnsAndRows (Vec n t)         _                 = '(n, 1)
  ColumnsAndRows _                 (Vec m t)         = '(1, m)
  ColumnsAndRows _                 _                 = '(1, 1)


-- | Smush a matrix type down into the correct vector, if applicable.
--
-- >>> :kind! Smush (Vec 4 (Vec 2 Int))
-- Smush (Vec 4 (Vec 2 Int)) :: *
-- = Vec 4 (Vec 2 Int)
--
-- >>> :kind! Smush (Vec 1 (Vec 4 Int))
-- Smush (Vec 1 (Vec 4 Int)) :: *
-- = Vec 4 Int
--
-- >>> :kind! Smush (Vec 2 (Vec 1 Float))
-- Smush (Vec 2 (Vec 1 Float)) :: *
-- = Vec 2 Float
type family Smush t where
  Smush (Vec 1 t) = Smush t
  Smush (Vec n t) = Vec n (Smush t)
  Smush t = t


-- | The individual component type of the vector.
--
-- >>> :kind! BaseType (Mat 4 4)
-- BaseType (Mat 4 4) :: *
-- = Float
--
-- >>> :kind! BaseType (Vec 4 Int)
-- BaseType (Vec 4 Int) :: *
-- = Int
type family BaseType t where
  BaseType (Vec n (Vec m t)) = t
  BaseType (Vec n t) = t
  BaseType t = t


-- | We can multiply scalars, vectors and matrices.
--
-- >>> :{
-- let sh :: Value (Out (Vec 4 Int)) -> GLSL ctx ()
--     sh = (.= mult (ivec4 0 1 2 3) (intVal 6))
-- in putStr $ glsl $ shader sh
-- >>> :}
-- out ivec4 a;
-- main () {
--   a = (ivec4(0, 1, 2, 3) * 6);
-- }
--
-- >>> :{
-- let vert
--       :: Value (Uniform (Mat 2 3))
--       -> Value (Uniform (Vec 3 Float))
--       -> Value (Out (Vec 2 Float))
--       -> GLSL ctx ()
--     vert umat uvec out = do
--       let mat = readFrom umat
--           vec = readFrom uvec
--       out .= mult vec mat
-- in putStr $ glsl $ shader vert
-- >>> :}
-- uniform mat2x3 a;
-- uniform vec3 b;
-- out vec2 c;
-- main () {
--   c = (b * a);
-- }
mult
  :: forall x y z n m xt yt.
     ( '(n, m)    ~ ColumnsAndRows x y
     , xt         ~ BaseType x
     , yt         ~ BaseType y
     , (xt == yt) ~ 'True
     , z          ~ Smush (Vec n (Vec m xt))
     )
  => Value x
  -> Value y
  -> Value z
mult = infx "*"
