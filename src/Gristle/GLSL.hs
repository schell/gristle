{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Gristle.GLSL where

import           Control.Monad.State                          (State, get, gets,
                                                               modify, put,
                                                               runState)
import           "prettyclass" Text.PrettyPrint.HughesPJClass (Pretty (..),
                                                               render, semi,
                                                               text, (<>))

import           Gristle.Linkage
import           Gristle.Syntax
import           Gristle.Types                                ()
import           Gristle.Vector


-- $setup
-- >>> :set -XScopedTypeVariables -XDataKinds -XFlexibleContexts
-- >>> :set -XTypeFamilies
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


class ShaderContext t where
  shaderContext :: String


data Vertex


instance ShaderContext Vertex where
  shaderContext = "vertex"


data Fragment


instance ShaderContext Fragment where
  shaderContext = "fragment"


type GLSL ctx = State GLSLData


toGLSL :: GLSLData -> String
toGLSL = render . mconcat . map pPrint . reverse . _glslStatements


runGLSL :: GLSL ctx a -> GLSLData -> (a, String)
runGLSL f dat0 = let (a, dat) = runState f dat0 in (a, toGLSL dat)


glsl :: GLSL ctx a -> String
glsl = snd . flip runGLSL (GLSLData [] allNames)


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
--   scoped "main (){" "}" $ do
--     let x = sin $ readFrom u
--     o .= x + 2.0
--     _ <- var $ vec2 x 3.0
--     return ()
-- >>> :}
-- out float a;
-- uniform float b;
-- main (){
--   a = (sin(b) + 2.0);
--   vec2 c;
--   c = vec2(sin(b), 3.0);
-- }
scoped :: String -> String -> GLSL ctx a -> GLSL ctx a
scoped starting closing f = do
  GLSLData statements names <- get
  let (a, dat) = runState f $ GLSLData [] names
      scope = ScopedStatements starting (reverse $ _glslStatements dat) closing
  put $ GLSLData (scope:statements) names
  return a


-- | Set the value of the output linkage.
(.=) :: Value (Out t) -> Value t -> GLSL ctx ()
(.=) = (statement .) . assign . castValue
infix 4 .=


-- | Make a new variable from a value.
--
-- >>> :{
-- let x :: Value Float = 0.5
--     y = 0.6
-- in putStr $ glsl $ shader $ \o -> do
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
  let b = render $ pPrint v
      start = concat ["if ( ", b, " ) {"]
  scoped start "}" f


-- | Early exit statement. Can only be used in fragment shaders.
discard :: GLSL Fragment ()
discard = statement $ ident "discard"


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


glFragColor :: GLSL Fragment (Value (Out (Vec 4 Float)))
glFragColor = getGlobal "gl_FragColor"


glFragCoord :: GLSL Fragment (Value (In (Vec 4 Float)))
glFragCoord = getGlobal "gl_FragCoord"


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
-- | Gristle's version of "main". A fun part of writing shaders in Gristle is
-- that you never have to declare @uniforms@, @in@s or @out@s. You simply write
-- a function that uses them and Gristle will provide them for you.
--
-- >>> :{
-- let frag :: Value (Uniform Float) -> Value (Out (Vec 4 Float)) -> GLSL ctx ()
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
class Shader ident where
  shader :: ident -> GLSL ctx ()


instance (HasLinkage t, Shader y) => Shader (Value t -> y) where
  shader = (declare >>=) . (shader .)


instance Shader (GLSL ctx ()) where
  shader f = scoped "main () {" "}" f
