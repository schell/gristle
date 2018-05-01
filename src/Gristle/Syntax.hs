{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Gristle.Syntax where

import           Control.Arrow                                ((&&&), (<<<),
                                                               (>>>))
import           Control.Monad.State                          (State, gets,
                                                               modify, runState)
import           Data.Char                                    (toLower)
import           Data.Fix                                     (Fix (..), cata,
                                                               cataM, hylo)
import           Data.Monoid                                  (Monoid (..),
                                                               (<>))
import           Data.Proxy                                   (Proxy (..))
import           Data.Ratio                                   (denominator,
                                                               numerator)
import           Data.Type.Equality
import           GHC.TypeLits
import           "prettyclass" Text.PrettyPrint.HughesPJClass (Doc, Pretty (..),
                                                               cat, comma, hsep,
                                                               parens,
                                                               prettyShow,
                                                               punctuate,
                                                               render, semi,
                                                               text, (<+>))
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PP


-- $setup
-- >>> :set -XTypeApplications -XDataKinds -XFlexibleContexts
-- >>> :set -XAllowAmbiguousTypes


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
              -- ^ A name for something like a variable or a function.
              | Unary String a
              -- ^ A unary op, mostly for casting.
              | InfixOp String a a
              -- ^ An infix operator, eg "+", "-", "*"
              | PostFixOp String a
              -- ^ A postfix operator, mostly for array access notation.
              | Call a [a]
              -- ^ A function call
              deriving (Show, Eq, Functor)


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
                 PostFixOp str a -> PostFixOp str a
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


--instance Pretty (Value t) where
--  pPrint = pPrint . unFix


ident :: String -> Value t
ident = Fix . Ident


infx :: String -> Value x -> Value y -> Value z
infx fn x y = Fix $ InfixOp fn (cast x) (cast y)


psfx :: Value x -> String -> Value z
psfx v s = Fix $ PostFixOp s (cast v)


prop :: forall x y. Value x -> String -> Value y
prop v s = infx "." v (ident s)


atIndex :: forall x y. Value x -> Integer -> Value y
atIndex v n = psfx v (concat ["[", show n, "]"])


call :: Value t -> [Value t] -> Value t
call fn param = Fix $ Call fn param


apply :: Value (x -> y) -> Value x -> Value y
apply = undefined


---- | Variable assignment without declaration.
----
---- >>> gPrint $ ident "somevar" `assign` floatVal 10
---- somevar = 10.0
--assign :: Value t -> Value t -> Value ()
--assign a b = cast $ infx "=" a b
--
--
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


-- | A paramorphism.
-- Traverse the structure folding an `a` with it.
para :: Functor f => (Fix f -> f a -> a) -> Fix f -> a
para r t = r t $ fmap (para r) $ unFix t


paraExpr :: (Value t -> Expr t a -> a) -> Value t -> a
paraExpr = para


---- | Hello
---- >>> go $ Fix $ Call (ident "blah") $ intVal 0
---- ()
--go = paraExpr $ \val -> \case
--  Call fn a ->
--  expr      -> [pPrint val]


--paramDocs = fmap (pure . pPrint) . unFix


-- | Pretty printing an Expr prints out its GLSL code.
--
---- >>> :{
---- let dostuff :: Value (Int -> Int -> Int)
----     dostuff = ident "dostuff"
----     curried :: Value (Int -> Int)
----     curried = call dostuff $ intVal $ -3
----     val :: Value Int
----     val = call curried $ intVal 6
---- in gPrint val
---- >>> :}
---- dostuff(-3, 6)
instance Pretty (Value t) where
  pPrint (Fix (Literal a))      = pPrint a
  pPrint (Fix (Ident name))     = text name
  pPrint (Fix (Unary op a))     = parens $ parens (text op) <> pPrint a
  pPrint (Fix (InfixOp op a b)) = (if op == "=" then id else parens) $ pPrint a <+> text op <+> pPrint b
  pPrint (Fix (PostFixOp op a)) = pPrint a <> text op
  pPrint (Fix (Call fn as))     = mconcat [ pPrint fn
                                          , parens $ hsep
                                                   $ punctuate comma
                                                   $ map pPrint as
                                          ]

----------------------------------------------------------------------------------
---- Vectors
----------------------------------------------------------------------------------
data Vec (n :: Nat) t


type family VecPrefix t where
  VecPrefix Bool    = "b"
  VecPrefix Int     = "i"
  VecPrefix Integer = "i"
  VecPrefix Word    = "u"
  VecPrefix Float   = ""
  VecPrefix Double  = ""


-- | Create a vec2.
--
-- >>> gPrint $ vec2 0.0 0.0
-- vec2(0.0, 0.0)
vec2
  :: forall t pre. (pre ~ VecPrefix t, KnownSymbol pre)
  => Value t
  -> Value t
  -> Value (Vec 2 t)
vec2 a b = cast $ call fn [a, b]
  where fn = ident $ symbolVal (Proxy @pre) ++ "vec2"


vec3
  :: forall t pre. (pre ~ VecPrefix t, KnownSymbol pre)
  => Value t
  -> Value t
  -> Value t
  -> Value (Vec 3 t)
vec3 a b c = cast $ call fn [a, b, c]
  where fn = ident $ symbolVal (Proxy @pre) ++ "vec3"


vec4
  :: forall t pre. (pre ~ VecPrefix t, KnownSymbol pre)
  => Value t
  -> Value t
  -> Value t
  -> Value t
  -> Value (Vec 4 t)
vec4 a b c d = cast $ call fn [a, b, c, d]
  where fn = ident $ symbolVal (Proxy @pre) ++ "vec4"


type Mat (n :: Nat) (m :: Nat) = Vec n (Vec m Float)


mat2
  :: forall n. (KnownNat n, n <= 4)
  => Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Mat 2 n)
mat2 a b = cast $ call fn [a, b]
  where fn = ident $ "mat2x" ++ show (natVal $ Proxy @n)


mat3
  :: forall n t. (KnownNat n, n <= 4)
  => Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Mat 3 n)
mat3 a b c = cast $ call fn [a, b, c]
  where fn = ident $ "mat3x" ++ show (natVal $ Proxy @n)


-- | Create a matrix from 4 vectors.
--
-- >>> :{
-- gPrint $ mat4 (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0)
--               (vec3 2.0 2.0 2.0) (vec3 3.0 3.0 3.0)
-- >>> :}
-- mat4x3(vec3(0.0, 0.0, 0.0), vec3(1.0, 1.0, 1.0), vec3(2.0, 2.0, 2.0), vec3(3.0, 3.0, 3.0))
mat4
  :: forall n t. (KnownNat n, n <= 4)
  => Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Vec n Float)
  -> Value (Mat 4 n)
mat4 a b c d = cast $ call fn [a, b, c, d]
  where fn = ident $ "mat4x" ++ show (natVal $ Proxy @n)


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
--     v = vec2 3 6
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
--     (x,y,z) = decomp m
-- in gPrint (a, b, c) >> gPrint (x, y, z)
-- >>> :}
-- ()
class ComponentsOf thing where
  type ComponentType thing
  type NumberOfComponents thing :: Nat
  decomp :: thing -> TupleOf (NumberOfComponents thing) (ComponentType thing)


instance (KnownNat n, 2 <= n, n <= 4) => ComponentsOf (Value (Vec (n :: Nat) t)) where
  type ComponentType (Value (Vec n t)) = Value t
  type NumberOfComponents (Value (Vec n t)) = n
  decomp v
    | Just Refl <- sameNat (Proxy @n) (Proxy @2) = (v `atIndex` 0, v `atIndex` 1)
    | Just Refl <- sameNat (Proxy @n) (Proxy @3) = (v `atIndex` 0, v `atIndex` 1, v `atIndex` 2)
    | Just Refl <- sameNat (Proxy @n) (Proxy @4) = (v `atIndex` 0, v `atIndex` 1, v `atIndex` 2, v `atIndex` 3)


--instance ( KnownNat n
--         , 2 <= n, n <= 4
--         , 2 <= m, m <= 4
--         ) => ComponentsOf (Value (Mat (n :: Nat) (m :: Nat))) where
--  type ComponentType (Value (Mat n m)) = Value (Vec m Float)
--  type NumberOfComponents (Value (Mat n m)) = n
--  decomp v
--    | Just Refl <- sameNat (Proxy @n) (Proxy @2) = (v `atIndex` 0, v `atIndex` 1)
--    | Just Refl <- sameNat (Proxy @n) (Proxy @3) = (v `atIndex` 0, v `atIndex` 1, v `atIndex` 1)
--    | Just Refl <- sameNat (Proxy @n) (Proxy @4) = (v `atIndex` 0, v `atIndex` 1, v `atIndex` 1, v `atIndex` 2)


--------------------------------------------------------------------------------
-- Uniforms, inputs attributes, outputs
--------------------------------------------------------------------------------
data Uniform t = Uniform
data In      t = In
data Out     t = Out


---- | Get the value of the input attribute.
----
---- >>> gPrint $ attributeValue $ In @"position" @(Vec 2 Float)
---- position
--attributeValue :: forall name t. KnownSymbol name => In name t -> Value t
--attributeValue = const $ ident (symbolVal $ Proxy @name)


---- | Get the value of the uniform.
----
---- >>> gPrint $ uniformValue $ Uniform @"projection" @(Vec 2 Float)
---- projection
--uniformValue :: forall name t. KnownSymbol name => Uniform name t -> Value t
--uniformValue = const $ ident (symbolVal $ Proxy @name)


--------------------------------------------------------------------------------
-- A GLSL program
--------------------------------------------------------------------------------

-- | $discussion
-- In a GLSL shader we need a number of special powers.
--
-- * We must be able to declare, pass around and read shader attributes (inputs).
-- * We must be able to declare, pass around and read shader uniforms   (inputs).
-- * We must be able to declare, pass around and write shader outputs
-- * In a fragment shader we must be able to short circuit (discard).
--
data GLSLData = GLSLData { _glslStatements :: [Term (Expr ())]
                         -- ^ A list of all glsl statements.
                         , _glslNames      :: [String]
                         -- ^ An infinite list of available ident names.
                         }


allNames :: [String]
allNames = let alphas = "abcdefghijklmnopqrstuvwxyz"
           in map pure alphas ++ [ [ch] ++ show n
                                 | n  <- [0 :: Integer ..]
                                 , ch <- alphas
                                 ]


data ShaderContext = Vertex
                   | Fragment
                   -- ...


type GLSL (x :: ShaderContext) (inputs :: [*]) = State GLSLData


toGLSL :: GLSLData -> String
toGLSL =
  render . (<> stop) . mconcat . punctuate stop . map pPrint . reverse . _glslStatements
  where stop = semi <> text "\n"


printGLSL :: GLSL x ts a -> IO ()
printGLSL = putStrLn . snd . flip runGLSL (GLSLData [] allNames)


runGLSL :: GLSL x ts a -> GLSLData -> (a, String)
runGLSL f dat0 = let (a, dat) = runState f dat0 in (a, toGLSL dat)


statement :: Term (Expr ()) -> GLSL x ts ()
statement term = modify $ \dat -> dat{ _glslStatements = term:_glslStatements dat }


-- | $example
--exampleVertexShader
--  :: Uniform (Mat 4 4 Float)
--  -> Uniform (Mat 4 4 Float)
--  -> In (Vec 2 Float)
--  -> In (Vec 4 Float)
--  -> Out (Vec 4 Float)
--  -> GLSL x ts ()
--exampleVertexShader projection modelview position color colorOut = do
--  let matrix    = projection * modelview
--      position4 = undefined
--  glPosition .= matrix * position4
--  colorOut   .= color
