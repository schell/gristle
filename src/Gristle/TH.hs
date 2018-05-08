{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
-- | To help with inference we write all our type classes with monomorphic types.
-- To help write these instances we use TemplateHaskell.
module Gristle.TH where

import           Data.Fix            (Fix (..))
import           Language.Haskell.TH

import           Gristle.Syntax


genNumInstance :: TypeQ -> ExpQ -> DecsQ
genNumInstance t frominteger = [d|
  instance Num (Value $t) where
    (+)         = infx "+"
    (-)         = infx "-"
    (*)         = infx "*"
    abs         = call (ident "abs") . pure
    signum      = call (ident "signum") . pure
    fromInteger = $frominteger --Value . Fix . Literal . $c . fromIntegral
  |]


genFractionalInstance :: TypeQ -> ExpQ -> DecsQ
genFractionalInstance t c = [d|
  instance Num (Value $t) => Fractional (Value $t) where
    fromRational = $c
    (/)          = infx "/"
  |]


genFloatingInstance :: TypeQ -> ExpQ -> DecsQ
genFloatingInstance t pifun = [d|
  instance Fractional (Value $t) => Floating (Value $t) where
    pi     = $pifun --Value $ Fix $ Literal $ $c pi
    exp    = call (ident "exp")   . pure
    log    = call (ident "log")   . pure
    sin    = call (ident "sin")   . pure
    cos    = call (ident "cos")   . pure
    asin   = call (ident "asin")  . pure
    acos   = call (ident "acos")  . pure
    atan   = call (ident "atan")  . pure
    sinh   = call (ident "sinh")  . pure
    cosh   = call (ident "cosh")  . pure
    asinh  = call (ident "asinh") . pure
    acosh  = call (ident "acosh") . pure
    atanh  = call (ident "atanh") . pure
    a ** b = call (ident "pow") [a, b]
 |]


genStuff :: (Applicative f, Monoid b1) => (a -> b2 -> f b1) -> [(a, b2)] -> f b1
genStuff f ts = mconcat <$> traverse (uncurry f) ts


componentQs :: [TypeQ]
componentQs = [ [t|2|], [t|3|], [t|4|] ]


floatingBaseTypes :: [TypeQ]
floatingBaseTypes = [ [t|Float|], [t|Double|] ]


constructFromRational :: ExpQ -> ExpQ
constructFromRational c =
  [e|\i -> $c $ (fromIntegral $ numerator i) / (fromIntegral $ denominator i) |]


floatingFromIntegral :: ExpQ
floatingFromIntegral = [e| Value . Fix . Literal . LitFloat . fromIntegral |]


vectorFrom :: ExpQ -> [ExpQ]
vectorFrom fromI =
  [ [e|\i -> let val = $fromI i in vec @2 val val|]
  , [e|\i -> let val = $fromI i in vec @3 val val val|]
  , [e|\i -> let val = $fromI i in vec @4 val val val val|]
  ]


floatingAndIntegralVecQs :: [(TypeQ, ExpQ, TypeQ)]
floatingAndIntegralVecQs = [ (t, c, n)
                           | t      <- (floatingBaseTypes ++ integralBaseTypes)
                           , (n, c) <- zip componentQs
                                           (vectorFrom [e| fromIntegral |])
                           ]


integralBaseTypes :: [TypeQ]
integralBaseTypes = [ [t|Int|], [t|Integer|], [t|Word|] ]
