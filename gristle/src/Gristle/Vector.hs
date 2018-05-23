{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Gristle.Vector where

import           Data.Proxy         (Proxy (..))
import           Data.Type.Equality
import           Data.Void          (absurd)
import           GHC.TypeLits

import           Gristle.Syntax


-- $setup
-- >>> :set -XScopedTypeVariables -XDataKinds -XTypeFamilies -XFlexibleContexts
-- >>> import Gristle.Syntax
-- >>> import Gristle.GLSL
-- >>> import Gristle.Types ()

--------------------------------------------------------------------------------
-- Vectors
--------------------------------------------------------------------------------
data Vec (n :: Nat) t


type family VecPrefix t where
  VecPrefix Bool    = "b"
  VecPrefix Int     = "i"
  VecPrefix Integer = "i"
  VecPrefix Word    = "u"
  VecPrefix Float   = ""
  VecPrefix Double  = ""
  VecPrefix (Vec n Float) = ""


-- | Type equations for helping us dynamically construct vectors.
--
-- >>> :kind! VecConstructor 2 Float
-- VecConstructor 2 Float :: *
-- = Value Float -> Value Float -> Value (Vec 2 Float)
--
-- >>> :kind! VecConstructor 3 Float
-- VecConstructor 3 Float :: *
-- = Value Float -> Value Float -> Value Float -> Value (Vec 3 Float)
type family VecConstructor (n :: Nat) t where
  VecConstructor 2 t = Value t -> Value t -> Value (Vec 2 t)
  VecConstructor 3 t = Value t -> Value t -> Value t -> Value (Vec 3 t)
  VecConstructor 4 t = Value t -> Value t -> Value t -> Value t -> Value (Vec 4 t)


vecFn
  :: forall n t pre. (pre ~ VecPrefix t, KnownSymbol pre, KnownNat n)
  => Value t
vecFn = ident $ concat [ symbolVal (Proxy @pre)
                       , "vec"
                       , show $ natVal (Proxy @n)
                       ]


-- | Create a vector with components where @2 <= number of components <= 4@.
--
vec
  :: forall n t. ( 2 <= n
                 , n <= 4
                 , KnownNat n
                 , KnownSymbol (VecPrefix t)
                 , IsLit t
                 )
  => VecConstructor n t
vec = case () of
  () | Just Refl <- sameNat (Proxy @n) (Proxy @2) ->
        \x y -> castValue $ call (vecFn @n) [x, y]
     | Just Refl <- sameNat (Proxy @n) (Proxy @3) ->
        \x y z -> castValue $ call (vecFn @n) [x, y, z]
     | Just Refl <- sameNat (Proxy @n) (Proxy @4) ->
        \x y z w -> castValue $ call (vecFn @n) [x, y, z, w]
     | otherwise -> absurd undefined


-- | Create a vec2.
--
-- >>> gPrint $ vec2 0.0 0.0
-- vec2(0.0, 0.0)
vec2 :: Value Float -> Value Float -> Value (Vec 2 Float)
vec2 = vec @2


ivec2 :: Value Int -> Value Int -> Value (Vec 2 Int)
ivec2 = vec @2


uvec2 :: Value Word -> Value Word -> Value (Vec 2 Word)
uvec2 = vec @2


bvec2 :: Value Bool -> Value Bool -> Value (Vec 2 Bool)
bvec2 = vec @2


vec3 :: Value Float -> Value Float -> Value Float -> Value (Vec 3 Float)
vec3 = vec @3


ivec3 :: Value Int -> Value Int -> Value Int -> Value (Vec 3 Int)
ivec3 = vec @3


uvec3 :: Value Word -> Value Word -> Value Word -> Value (Vec 3 Word)
uvec3 = vec @3


bvec3 :: Value Bool -> Value Bool -> Value Bool -> Value (Vec 3 Bool)
bvec3 = vec @3


vec4 :: Value Float -> Value Float -> Value Float -> Value Float -> Value (Vec 4 Float)
vec4 = vec @4


ivec4 :: Value Int -> Value Int -> Value Int -> Value Int -> Value (Vec 4 Int)
ivec4 = vec @4


uvec4 :: Value Word -> Value Word -> Value Word -> Value Word -> Value (Vec 4 Word)
uvec4 = vec @4


bvec4 :: Value Bool -> Value Bool -> Value Bool -> Value Bool -> Value (Vec 4 Bool)
bvec4 = vec @4


type Mat (n :: Nat) (m :: Nat) = Vec n (Vec m Float)


mat2
  :: forall n. (KnownNat n, n <= 4)
  => Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Mat 2 n)
mat2 a b = castValue $ call fn [a, b]
  where fn = ident $ "mat2x" ++ show (natVal $ Proxy @n)


mat3
  :: forall n. (KnownNat n, n <= 4)
  => Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Mat 3 n)
mat3 a b c = castValue $ call fn [a, b, c]
  where fn = ident $ "mat3x" ++ show (natVal $ Proxy @n)


-- | Create a matrix from 4 vectors.
--
-- >>> :{
-- gPrint $ mat4 (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0)
--               (vec3 2.0 2.0 2.0) (vec3 3.0 3.0 3.0)
-- >>> :}
-- mat4x3(vec3(0.0, 0.0, 0.0), vec3(1.0, 1.0, 1.0), vec3(2.0, 2.0, 2.0), vec3(3.0, 3.0, 3.0))
mat4
  :: forall n. (KnownNat n, n <= 4)
  => Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Mat 4 n)
mat4 a b c d = castValue $ call fn [a, b, c, d]
  where fn = ident $ "mat4x" ++ show (natVal $ Proxy @n)


-- | Vector division.
--
-- printGLSL $ do
--   a :: Value (Vec 2 Float) <- declare
--   _ <- var $ a ./ a
--   return ()
(./) :: Value (Vec n t) -> Value (Vec n t) -> Value (Vec n t)
(./) = infx "/"
infixl 7 ./


-- | Vector addition.
--
-- printGLSL $ do
--   a :: Value (Vec 2 Float) <- declare
--   _ <- var $ a .+ a
--   return ()
(.+) :: Value (Vec n t) -> Value (Vec n t) -> Value (Vec n t)
(.+) = infx "+"
infixl 6 .+


-- | Vector subtraction.
--
-- >>> :{
-- printGLSL $ do
--   a <- var $ vec2 0 1
--   _ <- var $ a .- a
--   return ()
-- >>> :}
-- vec2 a;
-- a = vec2(0.0, 1.0);
-- vec2 b;
-- b = (a - a);
(.-) :: Value (Vec n t) -> Value (Vec n t) -> Value (Vec n t)
(.-) = infx "-"
infixl 6 .-


type family TupleOf (n :: Nat) t where
  TupleOf 2 t = (t, t)
  TupleOf 3 t = (t, t, t)
  TupleOf 4 t = (t, t, t, t)
  TupleOf n t = TypeError (Text "TupleOf " :<>: ShowType n :<>: Text " " :<>: ShowType n
                                           :<>: Text "is not supported.")


-- | We can decompose some array types into their component values.
--
-- >>> :{
-- let v :: Value (Vec 2 Int)
--     v = ivec2 3 6
--     (x, y) = decomp v
-- in gPrint $ (x + 9, y - 9)
-- >>> :}
-- ((ivec2(3, 6)[0] + 9), (ivec2(3, 6)[1] - 9))
--
-- >>> :{
-- let a = vec4 0 1 2 3
--     b = vec4 4 5 6 7
--     c = vec4 8 9 0 1
--     m :: Value (Mat 3 4)
--     m = mat3 a b c
--     (_, _, z) = decomp m
-- in gPrint z
-- >>> :}
-- mat3x4(vec4(0.0, 1.0, 2.0, 3.0), vec4(4.0, 5.0, 6.0, 7.0), vec4(8.0, 9.0, 0.0, 1.0))[2]
--
-- >>> :{
-- gPrint $ decomp $ bvec3 true false true
-- >>> :}
-- (bvec3(true, false, true)[0], bvec3(true, false, true)[1],
--  bvec3(true, false, true)[2])
class ComponentsOf thing where
  type ComponentType thing
  type NumberOfComponents thing :: Nat
  decomp :: thing -> TupleOf (NumberOfComponents thing) (ComponentType thing)


numComps :: forall thing. KnownNat (NumberOfComponents thing) => Integer
numComps = natVal $ Proxy @(NumberOfComponents thing)


instance (KnownNat n, 2 <= n, n <= 4) => ComponentsOf (Value (Vec (n :: Nat) t)) where
  type ComponentType (Value (Vec n t)) = Value t
  type NumberOfComponents (Value (Vec n t)) = n
  decomp v
    | Just Refl <- sameNat (Proxy @n) (Proxy @2) = (v `atIndex` 0, v `atIndex` 1)
    | Just Refl <- sameNat (Proxy @n) (Proxy @3) = (v `atIndex` 0, v `atIndex` 1, v `atIndex` 2)
    | Just Refl <- sameNat (Proxy @n) (Proxy @4) = (v `atIndex` 0, v `atIndex` 1, v `atIndex` 2, v `atIndex` 3)
    | otherwise = absurd undefined
