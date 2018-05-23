{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Gristle.GLSL where

import           Control.Monad                                (forM_, void)
import           Control.Monad.State                          (MonadState (..),
                                                               State, get, gets,
                                                               modify, put,
                                                               runState)
import           Data.Proxy                                   (Proxy (..))
import           GHC.TypeLits                                 (KnownSymbol (..),
                                                               Symbol,
                                                               symbolVal)
import           Prelude                                      hiding (break)
import           "prettyclass" Text.PrettyPrint.HughesPJClass (Doc, Pretty (..),
                                                               nest, parens,
                                                               render, semi,
                                                               text, vcat,
                                                               (<+>), (<>))

import           Gristle.Linkage
import           Gristle.Syntax
import           Gristle.Types                                ()
import           Gristle.Vector


-- $setup
-- >>> :set -XScopedTypeVariables -XDataKinds -XFlexibleContexts
-- >>> :set -XTypeFamilies -XTypeApplications
-- >>> import Gristle.Linkage


--------------------------------------------------------------------------------
-- A GLSL ctx program
--------------------------------------------------------------------------------

-- | $discussion
-- In a GLSL ctx shader we need a number of special powers.
--
-- * We must be able to declare, pass around and read shader attributes (inputs).
-- * We must be able to declare, pass around and read shader uniforms   (inputs).
-- * We must be able to declare, pass around and write shader outputs
-- * In a fragment shader we must be able to short circuit (discard).
data Statement = Statement (Value ())
               -- ^ One line of an imperative program, just like you would expect.
               | ScopedStatements Doc [Statement] Doc
               -- ^ Many lines of a program in a new scope.


instance Pretty Statement where
  pPrint (Statement val) = pPrint val <> semi
  pPrint (ScopedStatements s vals e) =
    vcat [s, nest 2 (vcat $ map pPrint vals), e]


data GLSLData = GLSLData { _glslStatements :: [Statement]
                         -- ^ A list of all glsl statements.
                         , _glslNames      :: [String]
                         -- ^ An infinite list of available ident names.
                         }


allNames :: [String]
allNames = let alphas = "abcdefghijklmnopqrstuvwxyz"
           in map pure alphas ++ [ ch : show n
                                 | n  <- [0 :: Integer ..]
                                 , ch <- alphas
                                 ]


emptyGLSLData :: GLSLData
emptyGLSLData = GLSLData [] allNames


class ShaderContext t where
  shaderContext :: String


data Vertex


instance ShaderContext Vertex where
  shaderContext = "vertex"


data Fragment


instance ShaderContext Fragment where
  shaderContext = "fragment"


newtype GLSL ctx a = GLSL { unGLSL :: State GLSLData a }
                 deriving (Functor, Applicative, Monad)


deriving instance MonadState GLSLData (GLSL ctx)


toGLSL :: GLSLData -> String
toGLSL = render . vcat . map pPrint . reverse . _glslStatements


runGLSL :: GLSL ctx a -> GLSLData -> (a, String)
runGLSL f dat0 = let (a, dat) = runState (unGLSL f) dat0 in (a, toGLSL dat)


glsl :: GLSL ctx a -> String
glsl = snd . flip runGLSL emptyGLSLData


printGLSL :: GLSL ctx a -> IO ()
printGLSL = putStr . glsl


statement :: Value t -> GLSL ctx ()
statement term =
  let stmnt = Statement $ castValue term
  in modify $ \dat -> dat{ _glslStatements = stmnt:_glslStatements dat }


-- | Sometimes we don't want the last statement to be there. Pop! Is gone.
popStatement :: GLSL ctx ()
popStatement = do
  statements0 <- gets _glslStatements
  let statements = case statements0 of
        _:ss -> ss
        []   -> []
  modify $ \dat -> dat{ _glslStatements = statements }


-- | We can enter statements in a a new scope.
--
-- >>> :{
-- printGLSL $ do
--   o :: Value (Out Float) <- out
--   u <- uniform
--   vertex $ do
--     let x = sin $ readFrom u
--     o .= x + 2.0
--     _ <- var $ vec2 x 3.0
--     return ()
-- >>> :}
-- out float a;
-- uniform float b;
-- main () {
--   a = (sin(b) + 2.0);
--   vec2 c;
--   c = vec2(sin(b), 3.0);
-- }
scoped :: Doc -> Doc -> GLSL ctx a -> GLSL ctx a
scoped starting closing f = do
  GLSLData statements names <- get
  let (a, dat) = runState (unGLSL f) $ GLSLData [] names
      scope = ScopedStatements starting (reverse $ _glslStatements dat) closing
  put $ GLSLData (scope:statements) names
  return a


-- | Set the value of the output linkage.
(.=) :: Value (Out t) -> Value t -> GLSL ctx ()
(.=) = (statement .) . assign . castValue
infix 4 .=


-- | Set the value of the output linkage obtained by the monadic action.
set :: GLSL ctx (Value (Out t)) -> Value t -> GLSL ctx ()
set f v = f >>= (.= v)


-- | Make a new variable from a value.
--
-- >>> :{
-- let x :: Value Float = 0.5
--     y = 0.6
-- in putStr $ glsl $ fragment $ \o -> do
--      a <- var $ x + y
--      b <- var $ a * 600
--      o .= b
-- >>> :}
-- out float a;
-- main () {
--   float b;
--   b = (0.5 + 0.6);
--   float c;
--   c = (b * 600.0);
--   a = c;
-- }
var :: HasLinkage t => Value t -> GLSL ctx (Value t)
var val = do
  v <- declare
  statement $ castValue $ assign v val
  return v


-- | Only execute the glsl when the given Value Bool is 'true'.
--
-- >>> :{
-- putStr $ glsl $ do
--   ifthen true $ do
--     x <- var (3.0 :: Value Float)
--     _ <- var $ x + 2
--     return ()
-- >>> :}
-- if ( true ) {
--   float a;
--   a = 3.0;
--   float b;
--   b = (a + 2.0);
-- }
ifthen :: Value Bool -> GLSL ctx () -> GLSL ctx ()
ifthen v f = do
  let start = text "if (" <+> pPrint v <+> text ") {"
  scoped start (text "}") f


-- | Execute the first glsl when the given Value Bool is 'true',
-- otherwise execute the second.
--
-- >>> :{
-- putStr $ glsl $
--   ifthenelse
--     false
--     discard
--     $ do x <- var (3.0 :: Value Float)
--          _ <- var $ x + 2
--          return ()
-- >>> :}
-- if ( false ) {
--   discard;
-- }
-- else {
--   float a;
--   a = 3.0;
--   float b;
--   b = (a + 2.0);
-- }
ifthenelse :: Value Bool -> GLSL ctx () -> GLSL ctx () -> GLSL ctx ()
ifthenelse v t f = do
  let start = text "if (" <+> pPrint v <+> text ") {"
  scoped start (text "}") t
  scoped (text "else {") (text "}") f


-- | Execute only the glsl paired with the value that matches the value given as
-- the first parameter.
--
-- >>> :{
-- putStr $ glsl $
--   switch (intVal 5) [ (1, discard)
--                     , (2, discard)
--                     , (5, do x <- var 3
--                              _ <- var $ sin $ float x
--                              return ()
--                       )
--                     ]
-- >>> :}
-- switch ( 5 ) {
--   case ( 1 ) : {
--     discard;
--     break;
--   }
--   case ( 2 ) : {
--     discard;
--     break;
--   }
--   case ( 5 ) : {
--     int a;
--     a = 3;
--     float b;
--     b = sin(float(a));
--     break;
--   }
-- }
switch :: Integral t => Value t -> [(Value t, GLSL ctx ())] -> GLSL ctx ()
switch v cases = do
  let start = text "switch (" <+> pPrint v <+> text ") {"
  scoped start (text "}") $ forM_ cases $ \(val, f) -> do
    let cstart = text "case (" <+> pPrint val <+> text ") : {"
    scoped cstart (text "}") $ f >> break


-- | Early exit statement. Can only be used in fragment shaders.
discard :: GLSL Fragment ()
discard = statement $ ident "discard"


-- | Early exit from switch or if.
break :: GLSL ctx ()
break = statement $ ident "break"


-- | Declare an explicitly named binding.
named :: forall f t ctx. HasLinkage (f t) => String -> GLSL ctx (Value (f t))
named name = do
  let val = declaration (linkage @(f t)) name
  statement $ castValue val
  return $ ident name


fresh :: GLSL ctx String
fresh = do
  name:names <- gets _glslNames
  modify $ \dat -> dat{ _glslNames = names }
  return name


-- | Declare a linkage binding like Uniform, In or Out.
--
-- >>> :{
-- printGLSL $ do
--   color :: Value (Out (Vec 4 Float)) <- out
--   color .= vec4 1.0 1.0 0.0 1.0
-- >>> :}
-- out vec4 a;
-- a = vec4(1.0, 1.0, 0.0, 1.0);
declare :: forall t ctx. HasLinkage t => GLSL ctx (Value t)
declare = do
  name <- fresh
  let val = declaration (linkage @t) name
  statement $ castValue val
  return $ ident name


-- | `declare` parameterized over Uniform.
uniform :: HasLinkage t => GLSL ctx (Value (Uniform t))
uniform = declare


-- | `declare` parameterized over In.
attribute :: HasLinkage t => GLSL ctx (Value (In t))
attribute = declare


-- | `declare` parameterized over Out.
out :: HasLinkage t => GLSL ctx (Value (Out t))
out = declare


--------------------------------------------------------------------------------
-- Specially named things
--------------------------------------------------------------------------------
getGlobal :: HasLinkage (f t) => String -> GLSL ctx (Value (f t))
getGlobal name = do
  c <- named name
  popStatement
  return c


glPosition :: GLSL Vertex (Value (Out (Vec 4 Float)))
glPosition :: GLSL Vertex (Value (Out (Vec 4 Float))) = getGlobal "gl_Position"


glFragColor :: GLSL Fragment (Value (Out (Vec 4 Float)))
glFragColor = getGlobal "gl_FragColor"


glFragCoord :: GLSL Fragment (Value (In (Vec 4 Float)))
glFragCoord = getGlobal "gl_FragCoord"


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
newtype Link (name :: Symbol) t = Link { unLink :: t }


data ShaderLinkages ctx (as :: [*]) (us :: [*]) (os :: [*]) =
  ShaderLinkages { shaderLinkageAttribs  :: [Value ()]
                 , shaderLinkageUniforms :: [Value ()]
                 , shaderLinkageOuts     :: [Value ()]
                 , shaderLinkageSrc      :: String
                 , shaderLinkageCtx      :: String
                 }


instance Pretty (ShaderLinkages ctx as us os) where
  pPrint (ShaderLinkages as us os src ctx) =
    vcat [ text "Shader" <+> parens (text ctx) <+> text "linkages:"
         , nest 2 $ text "attribs:"
         , nest 4 $ vcat $ map pPrint as
         , nest 2 $ text "uniforms:"
         , nest 4 $ vcat $ map pPrint us
         , nest 2 $ text "outs:"
         , nest 4 $ vcat $ map pPrint os
         , nest 2 $ text "src:"
         , nest 4 $ vcat $ map text $ lines src
         ]


instance Show (ShaderLinkages ctx as us os) where
  show = render . pPrint


emptyLinkages :: forall ctx. ShaderContext ctx => ShaderLinkages ctx '[] '[] '[]
emptyLinkages = ShaderLinkages [] [] [] "" $ shaderContext @ctx


appendAttribute
  :: Value (In t)
  -> ShaderLinkages ctx as us os
  -> ShaderLinkages ctx (t:as) us os
appendAttribute v (ShaderLinkages as us os src x) =
  ShaderLinkages (castValue v : as) us os src x


appendUniform
  :: Value (Uniform t)
  -> ShaderLinkages ctx as us os
  -> ShaderLinkages ctx as (t:us) os
appendUniform v (ShaderLinkages as us os src x) =
  ShaderLinkages as (castValue v : us) os src x


appendOut
  :: Value (Out t)
  -> ShaderLinkages ctx as us os
  -> ShaderLinkages ctx as us (t:os)
appendOut v (ShaderLinkages as us os src x) =
  ShaderLinkages as us (castValue v : os) src x


-- | Gristle's version of "main". A fun part of writing shaders in Gristle is
-- that you never have to declare @uniforms@, @in@s or @out@s. You simply write
-- a function that uses them and Gristle will provide them for you.
--
-- >>> :{
-- let frag :: Value (Uniform Float) -> Value (Out (Vec 4 Float)) -> GLSL Fragment ()
--     frag utime fragColor = do
--       let r = sin $ readUniform utime
--       fragColor .= vec4 r 0.0 0.0 1.0
-- in printGLSL $ shader frag
-- >>> :}
-- uniform float a;
-- out vec4 b;
-- main () {
--   b = vec4(sin(a), 0.0, 0.0, 1.0);
-- }
class Shader t where
  type Ins      t :: [*]
  type Uniforms t :: [*]
  type Outs     t :: [*]
  type Ctx      t
  genShader :: t -> GLSL (Ctx t) (ShaderLinkages (Ctx t) (Ins t) (Uniforms t) (Outs t))


-- |
shader :: Shader i => i -> GLSL (Ctx i) ()
shader = void . genShader


-- | Monomorphic alias for 'shader'.
fragment :: (Shader i, Ctx i ~ Fragment) => i -> GLSL Fragment ()
fragment = shader


-- | Monomorphic alias for 'shader'.
vertex :: (Shader i, Ctx i ~ Vertex) => i -> GLSL Vertex ()
vertex = shader


instance (HasLinkage t, Shader y) => Shader (Value (In t) -> y) where
  type Ins      (Value (In t) -> y) = t : Ins y
  type Uniforms (Value (In t) -> y) = Uniforms y
  type Outs     (Value (In t) -> y) = Outs y
  type Ctx      (Value (In t) -> y) = Ctx y
  genShader f = do
    v  <- declare
    vs <- genShader $ f v
    return $ appendAttribute v vs


instance ( HasLinkage t
         , Shader y
         , KnownSymbol name
         ) => Shader (Link name (Value (In t)) -> y) where
  type Ins      (Link name (Value (In t)) -> y) = t : Ins y
  type Uniforms (Link name (Value (In t)) -> y) = Uniforms y
  type Outs     (Link name (Value (In t)) -> y) = Outs y
  type Ctx      (Link name (Value (In t)) -> y) = Ctx y
  genShader f = do
    v  <- named $ symbolVal $ Proxy @name
    vs <- genShader $ f $ Link v
    return $ appendAttribute v vs


instance ( HasLinkage t, Shader y) => Shader (Value (Uniform t) -> y) where
  type Ins      (Value (Uniform t) -> y) = Ins y
  type Uniforms (Value (Uniform t) -> y) = t : Uniforms y
  type Outs     (Value (Uniform t) -> y) = Outs y
  type Ctx      (Value (Uniform t) -> y) = Ctx y
  genShader f = do
    v  <- declare
    vs <- genShader $ f v
    return $ appendUniform v vs


instance ( HasLinkage t
         , Shader y
         , KnownSymbol name
         ) => Shader (Link name (Value (Uniform t)) -> y) where
  type Ins      (Link name (Value (Uniform t)) -> y) = Ins y
  type Uniforms (Link name (Value (Uniform t)) -> y) = t : Uniforms y
  type Outs     (Link name (Value (Uniform t)) -> y) = Outs y
  type Ctx      (Link name (Value (Uniform t)) -> y) = Ctx y
  genShader f = do
    v  <- named $ symbolVal $ Proxy @name
    vs <- genShader $ f $ Link v
    return $ appendUniform v vs


instance (HasLinkage t, Shader y) => Shader (Value (Out t) -> y) where
  type Ins      (Value (Out t) -> y) = Ins y
  type Uniforms (Value (Out t) -> y) = Uniforms y
  type Outs     (Value (Out t) -> y) = t : Outs y
  type Ctx      (Value (Out t) -> y) = Ctx y
  genShader f = do
    v  <- declare
    vs <- genShader $ f v
    return $ appendOut v vs


instance ( HasLinkage t
         , Shader y
         , KnownSymbol name
         ) => Shader (Link name (Value (Out t)) -> y) where
  type Ins      (Link name (Value (Out t)) -> y) = Ins y
  type Uniforms (Link name (Value (Out t)) -> y) = Uniforms y
  type Outs     (Link name (Value (Out t)) -> y) = t : Outs y
  type Ctx      (Link name (Value (Out t)) -> y) = Ctx y
  genShader f = do
    v  <- named $ symbolVal $ Proxy @name
    vs <- genShader $ f $ Link v
    return $ appendOut v vs


instance ShaderContext ctx => Shader (GLSL ctx ()) where
  type Ins      (GLSL ctx ()) = '[]
  type Uniforms (GLSL ctx ()) = '[]
  type Outs     (GLSL ctx ()) = '[]
  type Ctx      (GLSL ctx ()) = ctx
  genShader f = do
    scoped (text "void main () {") (text "}") f
    return emptyLinkages


-- | Generate the linkage for this shader.
linkages
  :: Shader t
  => t
  -> ShaderLinkages (Ctx t) (Ins t) (Uniforms t) (Outs t)
linkages t = let (l, s) = runGLSL (genShader t) emptyGLSLData
             in l{ shaderLinkageSrc = s }


-- $linkage
--
-- >>> :{
-- let vert :: Value (Uniform Float)
--          -> Value (In (Vec 2 Float))
--          -> Value (In (Vec 4 Float))
--          -> Value (Out (Vec 4 Float))
--          -> GLSL Vertex ()
--     vert red pos icolor ocolor = do
--       let (_, g, b, a) = decomp $ readFrom icolor
--           (x, y)       = decomp $ readFrom pos
--       set glPosition $ vec4 x y 0 1
--       ocolor .= vec4 (readFrom red) g b a
--     frag :: Value (In (Vec 4 Float))
--          -> Value (Out (Vec 4 Float))
--          -> GLSL Fragment ()
--     frag icolor ocolor = ocolor .= readFrom icolor
--     linkedVert
--       :: Value (Uniform Float)
--       -> Value (In (Vec 2 Float))
--       -> Value (In (Vec 4 Float))
--       -> Link "fcolor" (Value (Out (Vec 4 Float)))
--       -> GLSL Vertex ()
--     linkedVert red pos icolor colorOut = vert red pos icolor $ unLink colorOut
--     linkedFrag
--       :: Link "fcolor" (Value (In (Vec 4 Float)))
--       -> Value (Out (Vec 4 Float))
--       -> GLSL Fragment ()
--     linkedFrag = frag . unLink
-- in do print $ linkages linkedVert
--       print $ linkages linkedFrag
-- >>> :}
-- Shader (vertex) linkages:
--   attribs:
--     b
--     c
--   uniforms:
--     a
--   outs:
--     fcolor
--   src:
--     uniform float a;
--     in vec2 b;
--     in vec4 c;
--     out vec4 fcolor;
--     main () {
--       gl_Position = vec4(b[0], b[1], 0.0, 1.0);
--       fcolor = vec4(a, c[1], c[2], c[3]);
--     }
-- Shader (fragment) linkages:
--   attribs:
--     fcolor
--   uniforms:
--   outs:
--     a
--   src:
--     in vec4 fcolor;
--     out vec4 a;
--     main () {
--       a = fcolor;
--     }
