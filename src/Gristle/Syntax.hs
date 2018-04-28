{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Gristle.Syntax where

import           Control.Arrow                                ((<<<), (>>>))
import           Control.Monad.State                          (State, gets,
                                                               modify, runState)
import           Data.Char                                    (toLower)
import           Data.Fix                                     (Fix (..), hylo)
import           Data.Monoid                                  (Monoid (..),
                                                               (<>))
import           Data.Proxy                                   (Proxy (..))
import           Data.Ratio                                   (denominator,
                                                               numerator)
import           GHC.TypeLits
import           "prettyclass" Text.PrettyPrint.HughesPJClass (Pretty (..),
                                                               comma, hsep,
                                                               parens,
                                                               prettyShow,
                                                               punctuate,
                                                               render, semi,
                                                               text, (<+>))
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PP


-- $setup
-- >>> :set -XTypeApplications -XDataKinds


gPrint :: Pretty a => a -> IO ()
gPrint = putStrLn . prettyShow


data Lit = LitBool Bool
         | LitFloat Float
         | LitInt Int
         deriving (Show, Eq, Ord)


instance Pretty Lit where
  pPrint (LitBool b)  = text $ map toLower $ show $ b
  pPrint (LitFloat f) = PP.float f
  pPrint (LitInt i)   = PP.int i


data Expr t a = Literal Lit
              -- ^ A literal value.
              | Ident String
              -- ^ A name for something like a variable or a identtion.
              | Unary String a
              -- ^ A unary call for casting.
              | InfixOp String a a
              -- ^ An infix operator, eg "+", "-", "*"
              | Call a [a]
              -- ^ A identtion call
              deriving (Show, Eq, Functor)


-- | Pretty printing an Expr prints out its GLSL code.
--
-- >>> :{
-- gPrint @(Expr Int (Expr Int ())) $
--   Call (Ident "dostuff") [Literal $ LitInt (-3), Literal $ LitInt 6]
-- >>> :}
-- dostuff(-3, 6)
instance Pretty a => Pretty (Expr t a) where
  pPrint (Literal a)      = pPrint a
  pPrint (Ident name)     = text name
  pPrint (Unary op a)     = parens $ parens (text op) <> pPrint a
  pPrint (InfixOp op a b) = (if op == "=" then id else parens) $ pPrint a <+> text op <+> pPrint b
  pPrint (Call fn as)     = pPrint fn <> parens (hsep $ punctuate comma $ map pPrint as)


--------------------------------------------------------------------------------
-- Terms!
--------------------------------------------------------------------------------
type Term  = Fix
type Value t = Term (Expr t)


floatVal :: Float -> Value Float
floatVal = Fix . Literal . LitFloat


intVal :: Int -> Value Int
intVal = Fix . Literal . LitInt


boolVal :: Bool -> Value Bool
boolVal = Fix . Literal . LitBool


-- | GLSL's "false".
--
-- >>> gPrint false
-- false
false :: Value Bool
false = boolVal False


-- | GLSL's "true".
--
-- >>> gPrint true
-- true
true :: Value Bool
true = boolVal True


-- | This feels wrong.
castExpr :: forall y x a. Expr x a -> Expr y a
castExpr = \case Literal a       -> Literal a
                 Ident str       -> Ident str
                 Unary str a     -> Unary str a
                 InfixOp str a b -> InfixOp str a b
                 Call a as       -> Call a as


term2Expr :: forall x y. Value y -> Expr x (Value y)
term2Expr = castExpr . unFix


expr2Term :: forall y x. Expr x (Value y) -> Value y
expr2Term = Fix . castExpr


cast :: Value x -> Value y
cast = hylo expr2Term term2Expr


-- | Cast from some Num type to an "int".
--
-- >>> gPrint $ int $ floatVal 3.145
-- ((int)3.145)
int :: Num t => Value t -> Value Int
int = Fix . Unary "int" . cast


-- | Cast from some Num type to an "int".
--
-- >>> gPrint $ float $ intVal 20
-- ((float)20)
float :: Num t => Value t -> Value Float
float = Fix . Unary "float" . cast


instance Pretty (Value t) where
  pPrint = pPrint . unFix


ident :: String -> Value t
ident = Fix . Ident


infx :: String -> Value t -> Value t -> Value t
infx = ((Fix .) .) . InfixOp


call :: Value t -> [Value t] -> Value t
call fn params = Fix $ Call fn params


-- | Variable assignment without declaration.
--
-- >>> gPrint $ ident "somevar" `assign` floatVal 10
-- somevar = 10.0
assign :: Value t -> Value t -> Value ()
assign a b = cast $ infx "=" a b


-- | Terms can be expressed with numbers and operations on numbers.
--
-- >>> gPrint $ floatVal 0 + floatVal 1
-- (0.0 + 1.0)
--
-- >>> gPrint $ abs $ floatVal (-666)
-- abs(-666.0)
instance Num t => Num (Value t) where
  (+)         = infx "+"
  (-)         = infx "-"
  (*)         = infx "*"
  abs         = call (ident "abs") . pure
  signum      = call (ident "signum") . pure
  fromInteger = Fix . Literal . LitInt . fromIntegral


-- | Terms can be expressed with fractions!
--
-- >>> gPrint $ floatVal 20 / floatVal 6
-- (20.0 / 6.0)
instance Fractional t => Fractional (Value t) where
  fromRational r = Fix $ Literal $ LitFloat $ (/) (fromIntegral $ numerator r)
                                                  (fromIntegral $ denominator r)
  (/)            = infx "/"


-- | Terms can be expressed with floating point operations:
--
-- >>> gPrint $ pi @(Value Float)
-- 3.1415927
--
-- >>> gPrint $ floatVal 2 ** floatVal 4
-- pow(2.0, 4.0)
instance Floating t => Floating (Value t) where
  pi     = Fix $ Literal $ LitFloat pi
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

--------------------------------------------------------------------------------
-- A GLSL program
--------------------------------------------------------------------------------
newtype GLSLData = GLSLData { _glslStatements :: [Term (Expr ())] }


data ShaderContext = Vertex
                   | Fragment
                   -- ...


type GLSL (x :: ShaderContext) a = State GLSLData a


toGLSL :: GLSLData -> String
toGLSL =
  render . (<> stop) . mconcat . punctuate stop . map pPrint . reverse . _glslStatements
  where stop = semi <> text "\n"


printGLSL :: GLSL x a -> IO ()
printGLSL = putStrLn . snd . flip runGLSL (GLSLData [])


runGLSL :: GLSL x a -> GLSLData -> (a, String)
runGLSL f dat0 = let (a, dat) = runState f dat0 in (a, toGLSL dat)


statement :: Term (Expr ()) -> GLSL x ()
statement term = modify $ \dat -> dat{ _glslStatements = term:_glslStatements dat }


--------------------------------------------------------------------------------
-- Inputs, outputs, etc
--------------------------------------------------------------------------------
newtype In a = In { unIn :: Value a }
newtype Out a = Out { unOut :: Value a }


readFrom :: In a -> Value a
readFrom = unIn


--------------------------------------------------------------------------------
-- Vectors
--------------------------------------------------------------------------------
data Vec (n :: Nat) t


-- | Create a vec2.
--
-- >>> gPrint $ vec2 0.0 0.0
-- vec2(0.0, 0.0)
vec2 :: Value t -> Value t -> Value (Vec 2 t)
vec2 a b = cast $ call (ident "vec2") [a, b]


vec3 ::  Value t -> Value t -> Value t -> Value (Vec 3 t)
vec3 a b c = cast $ call (ident "vec3") [a, b, c]


vec4 :: Value t -> Value t -> Value t -> Value t -> Value (Vec 4 t)
vec4 a b c d = cast $ call (ident "vec4") [a, b, c, d]


type Mat (n :: Nat) (m :: Nat) t = Vec n (Vec m t)


mat2
  :: forall n t. (KnownNat n, n <= 4)
  => Value (Vec n t)
  -> Value (Vec n t)
  -> Value (Mat 2 n t)
mat2 a b = cast $ call fn [a, b]
  where fn = ident $ "mat2x" ++ show (natVal $ Proxy @n)


mat3
  :: forall n t. (KnownNat n, n <= 4)
  => Value (Vec n t)
  -> Value (Vec n t)
  -> Value (Vec n t)
  -> Value (Mat 3 n t)
mat3 a b c = cast $ call fn [a, b, c]
  where fn = ident $ "mat3x" ++ show (natVal $ Proxy @n)


mat4
  :: forall n t. (KnownNat n, n <= 4)
  => Value (Vec n t)
  -> Value (Vec n t)
  -> Value (Vec n t)
  -> Value (Vec n t)
  -> Value (Mat 4 n t)
mat4 a b c d = cast $ call fn [a, b, c, d]
  where fn = ident $ "mat4x" ++ show (natVal $ Proxy @n)
