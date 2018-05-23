{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Gristle.Linkage where

import           Data.Proxy          (Proxy (..))
import           Data.Type.Bool
import           Data.Type.Equality  hiding (apply)
import           Data.Typeable       (Typeable)
import           GHC.TypeLits

import           Gristle.Syntax
import           Gristle.TH
import           Gristle.Vector


-- $setup
-- >>> import Gristle.GLSL


data Uniform t
data In      t
data Out     t


type family IsLinkageType (t :: * -> *) where
  IsLinkageType Uniform = 'True
  IsLinkageType In      = 'True
  IsLinkageType Out     = 'True


type IsLinkage t = (Typeable t, IsLinkageType t ~ 'True)


type family CanReadFrom (f :: * -> *) t where
  CanReadFrom Uniform t = t
  CanReadFrom In t      = t
  CanReadFrom f t       = TypeError (Text "Cannot read from" :<>: ShowType f)


-- | Read the value out of an input.
readFrom :: Value (f t) -> Value (CanReadFrom f t)
readFrom = castValue


-- | Less polymorphic version of 'readFrom'.
readUniform :: Value (Uniform t) -> Value t
readUniform = readFrom


-- | Another less polymorphic version of 'readFrom'.
readIn :: Value (In t) -> Value t
readIn = readFrom


-- | Declare a linkage binding like Uniform, In or Out.
--
-- >>> :{
-- printGLSL $ do
--   bvec <- out
--   bvec .= bvec2 true false
-- >>> :}
-- out bvec2 a;
-- a = bvec2(true, false);
--
-- >>> :{
-- printGLSL $ do
--   m <- out
--   m .= mat2 (vec3 0 1 2) (vec3 3 4 5)
-- >>> :}
-- out mat2x3 a;
-- a = mat2x3(vec3(0.0, 1.0, 2.0), vec3(3.0, 4.0, 5.0));
class HasLinkage t where
  linkage :: String


instance HasLinkage Word where linkage = "uint"
instance HasLinkage Int where linkage = "int"
instance HasLinkage Integer where linkage = "int"
instance HasLinkage Float where linkage = "float"
instance HasLinkage Double where linkage = "float"
instance HasLinkage Bool where linkage = "bool"


$(fmap concat $ sequence $ do
      t <- floatingBaseTypes ++ integralBaseTypes ++ pure [t| Bool |]
      n <- componentQs
      return $
        [d| instance (pre ~ VecPrefix $t) => HasLinkage (Vec $n $t) where
              linkage = concat [ symbolVal (Proxy @pre)
                               , "vec"
                               , show $ natVal (Proxy @($n))
                               ]
        |]
 )


instance ( KnownNat n
         , KnownNat m
         , Typeable t
         , (t == Float || t == Double) ~ 'True
         ) => HasLinkage (Vec n (Vec m t)) where
  linkage = let vn = natVal $ Proxy @n
                vm = natVal $ Proxy @m
            in "mat" ++ if vn == vm
                        then show vn
                        else concat [ show (natVal $ Proxy @n)
                                    , "x"
                                    , show (natVal $ Proxy @m)
                                    ]


instance HasLinkage t => HasLinkage (Uniform t) where
  linkage = "uniform " ++ linkage @t
instance HasLinkage t => HasLinkage (In t) where
  linkage = "in " ++ linkage @t
instance HasLinkage t => HasLinkage (Out t) where
  linkage = "out " ++ linkage @t
