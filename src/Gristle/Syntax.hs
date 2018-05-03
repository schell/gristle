{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
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
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Gristle.Syntax where

import           Control.Category                             (Category (..),
                                                               id, (.))
import           Control.Monad.State                          (State, get, gets,
                                                               modify, put,
                                                               runState)
import           Data.Char                                    (toLower)
import           Data.Fix                                     (Fix (..), hylo)
import           Data.Monoid                                  (Monoid (..),
                                                               (<>))
import           Data.Proxy                                   (Proxy (..))
import           Data.Ratio                                   (denominator,
                                                               numerator)
import           Data.Type.Bool
import           Data.Type.Equality                           hiding (apply)
import           Data.Typeable                                (Typeable, eqT)
import           Data.Void                                    (absurd)
import           GHC.TypeLits
import           Prelude                                      hiding (id, (.))
import           "prettyclass" Text.PrettyPrint.HughesPJClass (Pretty (..),
                                                               comma, hsep,
                                                               parens,
                                                               prettyShow,
                                                               punctuate, semi,
                                                               text, (<+>))
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PP


-- $setup
-- >>> :set -XTypeApplications -XDataKinds -XFlexibleContexts
-- >>> :set -XAllowAmbiguousTypes -XTypeFamilies


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
  pPrint (LitBool b)  = text $ map toLower $ show $ b
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
                 Unary str a     -> Unary str a
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
-- ((int)3.145)
int :: Num t => Value t -> Value Int
int = Value . Fix . Unary "int" . castFix . unValue


-- | Cast from some Num type to an "int".
--
-- >>> gPrint $ float $ intVal 20
-- ((float)20)
float :: Num t => Value t -> Value Float
float = Value . Fix . Unary "float" . castFix . unValue


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
-- in gPrint $ apply (apply f 6.0) 7.0
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
-- in gPrint $ f <:> 6.0 <:> 7.0
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


-- | Values can be expressed with numbers and operations on numbers.
--
-- >>> gPrint $ floatVal 0 + floatVal 1
-- (0.0 + 1.0)
--
-- >>> gPrint $ abs $ floatVal (-666)
-- abs(-666.0)
--
-- >>> gPrint $ abs (0 :: Value Float)
-- abs(0.0)
--
-- >>> gPrint $ abs (0 :: Value Int)
-- abs(0)
instance (IsLit t, Num t) => Num (Value t) where
  (+)         = infx "+"
  (-)         = infx "-"
  (*)         = infx "*"
  abs         = call (ident "abs") . pure
  signum      = call (ident "signum") . pure
  fromInteger = Value . Fix . Literal . lit . fromIntegral


-- | Values can be expressed with fractions!
--
-- >>> gPrint $ floatVal 20 / floatVal 6
-- (20.0 / 6.0)
instance (Fractional t, IsLit t) => Fractional (Value t) where
  fromRational r = Value $ Fix $ Literal $ lit @t $ (/) (fromIntegral $ numerator r)
                                                        (fromIntegral $ denominator r)
  (/)            = infx "/"


-- | Values can be expressed with floating point operations:
--
-- >>> gPrint $ pi @(Value Float)
-- 3.1415927
--
-- >>> gPrint $ floatVal 2 ** floatVal 4
-- pow(2.0, 4.0)
instance (Floating t, IsLit t) => Floating (Value t) where
  pi     = Value $ Fix $ Literal $ lit @t pi
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
para :: Functor f => (Fix f -> f a -> a) -> Fix f -> a
para r t = r t $ fmap (para r) $ unFix t


-- | Pretty printing an Expr prints out its GLSL code.
instance Pretty (Value t) where
  pPrint (Value (Fix (Literal a)))      = pPrint a
  pPrint (Value (Fix (Ident name)))     = text name
  pPrint (Value (Fix (Unary op a)))     = parens $ parens (text op) <> pPrint (Value a)
  pPrint (Value (Fix (PostFixOp op a))) = pPrint (Value a) <> text op
  pPrint (Value (Fix (InfixOp op a b))) =
    let f = if op == "=" then id else parens
    in f $ pPrint (Value a) <+> text op <+> pPrint (Value b)
  pPrint (Value (Fix (Call fn as)))     =
    mconcat [ pPrint $ Value fn
            , parens $ hsep $ punctuate comma $ map (pPrint . Value) as
            ]


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


-- | Create a vec2.
--
-- >>> gPrint $ vec2 0.0 0.0
-- vec2(0.0, 0.0)
vec2
  :: forall t pre. (pre ~ VecPrefix t, KnownSymbol pre)
  => Value t
  -> Value t
  -> Value (Vec 2 t)
vec2 a b = castValue $ call fn [a, b]
  where fn = ident $ symbolVal (Proxy @pre) ++ "vec2"


vec3
  :: forall t pre. (pre ~ VecPrefix t, KnownSymbol pre)
  => Value t
  -> Value t
  -> Value t
  -> Value (Vec 3 t)
vec3 a b c = castValue $ call fn [a, b, c]
  where fn = ident $ symbolVal (Proxy @pre) ++ "vec3"


vec4
  :: forall t pre. (pre ~ VecPrefix t, KnownSymbol pre)
  => Value t
  -> Value t
  -> Value t
  -> Value t
  -> Value (Vec 4 t)
vec4 a b c d = castValue $ call fn [a, b, c, d]
  where fn = ident $ symbolVal (Proxy @pre) ++ "vec4"


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
--     (_, _, z) = decomp m
-- in gPrint z
-- >>> :}
-- mat3x4(vec4(0.0, 1.0, 2.0, 3.0), vec4(4.0, 5.0, 6.0, 7.0), vec4(8.0, 9.0, 0.0, 1.0))[2]
--
-- >>> :{
-- gPrint $ decomp $ vec3 true false true
-- >>> :}
-- (bvec3(true, false, true)[0], bvec3(true, false, true)[1],
--  bvec3(true, false, true)[2])
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
    | otherwise = absurd undefined


----------------------------------------------------------------------------------
---- Linkage! Uniforms, inputs attributes, outputs
----------------------------------------------------------------------------------
data Uniform t
data In      t
data Out     t


type family IsLinkageType (t :: * -> *) where
  IsLinkageType Uniform = 'True
  IsLinkageType In      = 'True
  IsLinkageType Out     = 'True


type IsLinkage t = (Typeable t, IsLinkageType t ~ 'True)


readFrom
  :: forall f t. (f == Uniform || f == In) ~ 'True
  => Value (f t)
  -> Value t
readFrom = castValue

-- | Declare a linkage binding like Uniform, In or Out.
--
-- >>> :{
-- printGLSL $ do
--   bvec <- out
--   bvec .= vec2 true false
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


instance {-# OVERLAPS #-}
         ( KnownNat n
         , VecPrefix t ~ pre
         , KnownSymbol pre
         , HasLinkage t
         , IsLit t
         , Typeable t
         ) => HasLinkage (Vec n t) where
  linkage = concat [ symbolVal (Proxy @pre)
                   , "vec"
                   , show $ natVal (Proxy @n)
                   ]


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
data Statement = Statement (Value ())
               -- ^ One line of an imperative program, just like you would expect.
               | ScopedStatements String [Statement] String
               -- ^ Many lines of a program in a new scope.


instance Pretty Statement where
  pPrint (Statement val) = pPrint val <> semi <> text "\n"
  pPrint (ScopedStatements s vals e) = mconcat [ text s
                                               , text "\n"
                                               , mconcat $ map ((text "  " <>) . pPrint) vals
                                               , text e
                                               , text "\n"
                                               ]


data GLSLData = GLSLData { _glslStatements :: [Statement]
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


type GLSL = State GLSLData


toGLSL :: GLSLData -> String
toGLSL = PP.render . mconcat . map pPrint . reverse . _glslStatements


runGLSL :: GLSL a -> GLSLData -> (a, String)
runGLSL f dat0 = let (a, dat) = runState f dat0 in (a, toGLSL dat)


glsl :: GLSL a -> String
glsl = snd . flip runGLSL (GLSLData [] allNames)


printGLSL :: GLSL a -> IO ()
printGLSL = putStr . glsl


statement :: Value t -> GLSL ()
statement term =
  let stmnt = Statement $ castValue term
  in modify $ \dat -> dat{ _glslStatements = stmnt:_glslStatements dat }


-- | We can enter statements in a a new scope.
--
-- >>> :{
-- printGLSL $ do
--   o <- out
--   u <- uniform
--   scoped "main (){" "}" $ do
--     let x = sin $ readFrom u
--     o .= x + 2.0
-- >>> :}
-- out float a;
-- uniform float b;
-- main (){
--   a = (sin(b) + 2.0);
-- }
scoped :: String -> String -> GLSL a -> GLSL a
scoped starting closing f = do
  GLSLData statements names <- get
  let (a, dat) = runState f $ GLSLData [] names
      scope = ScopedStatements starting (_glslStatements dat) closing
  put $ GLSLData (scope:statements) names
  return a


(.=) :: Value (Out t) -> Value t -> GLSL ()
(.=) var val = statement $ assign (castValue var) val
infix 4 .=


fresh :: GLSL String
fresh = do
  name:names <- gets _glslNames
  modify $ \dat -> dat{ _glslNames = names }
  return name


-- | Declare a linkage binding like Uniform, In or Out.
--
-- >>> :{
-- printGLSL $ do
--   color <- out
--   color .= vec4 1.0 1.0 0.0 1.0
-- >>> :}
-- out vec4 a;
-- a = vec4(1.0, 1.0, 0.0, 1.0);
declare :: forall t. HasLinkage t => GLSL (Value t)
declare = do
  name <- fresh
  let val = declaration (linkage @t) name
  statement $ castValue val
  return $ ident name


-- | `declare` parameterized over Uniform.
uniform :: HasLinkage t => GLSL (Value (Uniform t))
uniform = declare


-- | `declare` parameterized over In.
attribute :: HasLinkage t => GLSL (Value (In t))
attribute = declare


-- | `declare` parameterized over Out.
out :: HasLinkage t => GLSL (Value (Out t))
out = declare


-- | $example
-- https://thebookofshaders.com/02/
--
-- >>> printGLSL $ bos02FragmentShader $ ident "gl_FragColor"
-- gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);
bos02FragmentShader
  :: Value (Out (Vec 4 Float))
  -> GLSL ()
bos02FragmentShader fragColor = fragColor .= vec4 1 0 1 1


-- | https://thebookofshaders.com/03/
--
-- >>> printGLSL $ bos03FragmentShader (ident "u_time") (ident "gl_FragColor")
-- gl_FragColor = vec4(abs(sin(u_time)), 0.0, 0.0, 1.0);
bos03FragmentShader
  :: Value (Uniform Float)
  -> Value (Out (Vec 4 Float))
  -> GLSL ()
bos03FragmentShader utime fragColor = do
  let r = abs $ sin $ readFrom utime
  fragColor .= vec4 r 0 0 1


-- | Gristle's version of "main".
--
-- >>> :{
-- let frag :: Value (Uniform Float) -> Value (Out (Vec 4 Float)) -> GLSL ()
--     frag utime fragColor = do
--       let r = sin $ readFrom utime
--       fragColor .= vec4 r 0.0 0.0 1.0
-- in printGLSL $ shader frag
-- >>> :}
-- uniform float a;
-- out vec4 b;
-- main () {
--   b = vec4(sin(a), 0.0, 0.0, 1.0);
-- }
class Shader ident where
  shader :: ident -> GLSL ()


instance Shader () where
  shader () = return ()


instance (HasLinkage t, Shader y) => Shader (Value t -> y) where
  shader f = do
    val <- declare
    shader $ f val


instance Shader (GLSL ()) where
  shader f = scoped "main () {" "}" f
