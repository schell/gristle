{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
-- | This module provides the operations and combinators needed to construct
-- GLSL values in Haskell. It's not meant to be used by library consumers, only
-- those brave souls out to write their own EDSLs. There's lots of funky joe
-- monkey contained within. There's no `unsafeCoerce` and friends, everything is
-- hunky dory in Haskell-land but the author makes no guarantees about the safety
-- of the generated GLSL. If it's safety you want __don't import this module__.
-- The top level "Gristle" module are the droids you're looking for.
module Gristle.Syntax where


import           Control.Category                             (Category (..),
                                                               id, (.))
import           Data.Char                                    (toLower)
import           Data.Fix                                     (Fix (..), hylo)
import           Data.Monoid                                  (Monoid (..),
                                                               (<>))
import           Data.Type.Equality                           hiding (apply)
import           Data.Typeable                                (Typeable, eqT)
import           Data.Void                                    (absurd)
import           Prelude                                      hiding (id, (.))
import           "prettyclass" Text.PrettyPrint.HughesPJClass (Pretty (..),
                                                               comma, hsep,
                                                               parens,
                                                               prettyShow,
                                                               punctuate, text,
                                                               (<+>))
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PP


-- $setup
-- >>> :set -XTypeApplications -XDataKinds -XFlexibleContexts
-- >>> :set -XAllowAmbiguousTypes -XTypeFamilies -XScopedTypeVariables
-- >>> import Gristle.Types ()


gPrint :: Pretty a => a -> IO ()
gPrint = putStrLn . prettyShow


--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------
data Lit t = LitBool Bool
           | LitFloat Float
           | LitInt Int
           deriving (Show, Eq, Ord)


castLit :: Lit a -> Lit b
castLit = \case (LitBool b)  -> LitBool b
                (LitFloat f) -> LitFloat f
                (LitInt i)   -> LitInt i


instance Pretty (Lit t) where
  pPrint (LitBool b)  = text $ map toLower $ show b
  pPrint (LitFloat f) = PP.float f
  pPrint (LitInt i)   = PP.int i


type family IsLiteralType t where
  IsLiteralType Word   = 'True
  IsLiteralType Int    = 'True
  IsLiteralType Integer= 'True
  IsLiteralType Float  = 'True
  IsLiteralType Double = 'True
  IsLiteralType Bool   = 'True


type IsLit t = (Typeable t, IsLiteralType t ~ 'True)


lit :: forall t. IsLit t => t -> Lit t
lit n | Just (Refl :: t :~: Word   ) <- eqT = LitInt $ fromIntegral n
      | Just (Refl :: t :~: Int    ) <- eqT = LitInt n
      | Just (Refl :: t :~: Integer) <- eqT = LitInt $ fromIntegral n
      | Just (Refl :: t :~: Float  ) <- eqT = LitFloat n
      | Just (Refl :: t :~: Double ) <- eqT = LitFloat $ realToFrac n
      | Just (Refl :: t :~: Bool   ) <- eqT = LitBool n
      | otherwise = absurd undefined


data Expr t a = Literal (Lit t)
              -- ^ A literal value.
              | Ident String
              -- ^ A name for something like a variable or a function.
              | InfixOp String a a
              -- ^ An infix operator, eg "+", "-", "*"
              | PostFixOp String a
              -- ^ A postfix operator, mostly for array access notation.
              | Call a [a]
              -- ^ A function call
              deriving (Show, Eq, Functor)


--------------------------------------------------------------------------------
-- Values!
--------------------------------------------------------------------------------
newtype Value t = Value { unValue :: Fix (Expr t) }


-- | Create a literal value from a couple valid types.
--
-- >>> gPrint $ litVal @Float 0
-- 0.0
--
-- >>> gPrint $ litVal 0.0
-- 0.0
litVal :: IsLit t => t -> Value t
litVal = Value . Fix . Literal . lit


floatVal :: Float -> Value Float
floatVal = litVal


intVal :: Int -> Value Int
intVal = litVal


-- | GLSL's "false".
--
-- >>> gPrint false
-- false
false :: Value Bool
false = litVal False


-- | GLSL's "true".
--
-- >>> gPrint true
-- true
true :: Value Bool
true = litVal True


-- | This feels wrong. It's my body oh well.
castExpr :: forall y x a. Expr x a -> Expr y a
castExpr = \case Literal a       -> Literal $ castLit a
                 Ident str       -> Ident str
                 InfixOp str a b -> InfixOp str a b
                 PostFixOp str a -> PostFixOp str a
                 Call a as       -> Call a as


term2Expr :: forall x y. Fix (Expr y) -> Expr x (Fix (Expr y))
term2Expr = castExpr . unFix


expr2Value :: forall y x. Expr x (Fix (Expr y)) -> Fix (Expr y)
expr2Value = Fix . castExpr


castFix :: Fix (Expr x) -> Fix (Expr y)
castFix = hylo expr2Value term2Expr


castValue :: Value x -> Value y
castValue = Value . castFix . unValue

-- | Cast from some Num type to an "int".
--
-- >>> gPrint $ int $ floatVal 3.145
-- int(3.145)
int :: Num t => Value t -> Value Int
int = call (ident "int") . pure . castValue


-- | Cast from some Num type to an "int".
--
-- >>> gPrint $ float $ intVal 20
-- float(20)
float :: Num t => Value t -> Value Float
float = call (ident "float") . pure . castValue


ident :: String -> Value t
ident = Value . Fix . Ident


declaration :: String -> String -> Value t
declaration link name = ident $ unwords [link, name]


infx :: String -> Value x -> Value y -> Value z
infx fn (Value x) (Value y) = Value $ Fix $ InfixOp fn (castFix x) (castFix y)


psfx :: Value x -> String -> Value z
psfx (Value v) s = Value $ Fix $ PostFixOp s (castFix v)


prop :: forall x y. Value x -> String -> Value y
prop v s = infx "." v (ident s)


atIndex :: forall x y. Value x -> Integer -> Value y
atIndex v n = psfx v (concat ["[", show n, "]"])


call :: Value t -> [Value t] -> Value t
call (Value fn) = Value . Fix . Call fn . map unValue


-- | Woah! We can do this?
-- >>> :{
-- let f :: Value (Float -> Float -> Float)
--     f = ident "adder"
-- in gPrint $ apply (apply f $ floatVal 6.0) $ floatVal 7.0
-- >>> :}
-- adder(6.0, 7.0)
--
-- Unfortunately Value is not a Functor and hence not Applicative.
apply :: Value (x -> y) -> Value x -> Value y
apply (Value f) (Value x) =
  Value $ case unFix f of
    Call a as -> castFix $ Fix $ Call a $ as ++ [castFix x]
    _         -> Fix $ Call (castFix f) [castFix x]


-- | Infix version of 'apply'.
-- >>> :{
-- let f :: Value (Float -> Float -> Float)
--     f = ident "adder"
-- in gPrint $ f <:> floatVal 6.0 <:> floatVal 7.0
-- >>> :}
-- adder(6.0, 7.0)
(<:>) :: Value (x -> y) -> Value x -> Value y
(<:>) = apply
infixl 4 <:>


-- | Variable assignment without declaration.
--
-- >>> gPrint $ ident "somevar" `assign` floatVal 10
-- somevar = 10.0
assign :: Value t -> Value t -> Value ()
assign a b = castValue $ infx "=" a b


-- | Pretty printing an Expr prints out its GLSL code.
instance Pretty (Value t) where
  pPrint (Value (Fix (Literal a)))      = pPrint a
  pPrint (Value (Fix (Ident name)))     = text name
  pPrint (Value (Fix (PostFixOp op a))) = pPrint (Value a) <> text op
  pPrint (Value (Fix (InfixOp op a b))) =
    let f = if op == "=" then id else parens
    in f $ pPrint (Value a) <+> text op <+> pPrint (Value b)
  pPrint (Value (Fix (Call fn as)))     =
    mconcat [ pPrint $ Value fn
            , parens $ hsep $ punctuate comma $ map (pPrint . Value) as
            ]
