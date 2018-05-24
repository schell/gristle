{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module TH where

import           Control.Exception     (assert)
import           Control.Monad         (when)
import qualified Data.Foldable         as F
import           Data.Proxy            (Proxy (..))
import qualified Data.Vector.Storable  as S
import           Data.Vector.Unboxed   (Unbox, Vector)
import qualified Data.Vector.Unboxed   as V
import           Foreign.C.String      (withCString)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr           (castPtr, nullPtr)
import           Foreign.Storable      (Storable (..))
import           GHC.TypeLits          (KnownNat, natVal)
import           Data.Type.Equality                           hiding (apply)
import           Data.Typeable                                (Typeable, eqT)
import           Graphics.GL
import           Linear

import           Gristle
import           Gristle.GLSL
import           Gristle.Syntax
import           Gristle.Vector


data a :& b = a :& b
infixr 8 :&


class ShaderFunction t where
  type ShaderFunctionType t
  mkShaderFunction :: t -> ShaderFunctionType t


instance ShaderFunction () where
  type ShaderFunctionType () = ()
  mkShaderFunction () = ()


instance (ShaderFunction a, ShaderFunction b) => ShaderFunction (a :& b) where
  type ShaderFunctionType (a :& b) = ShaderFunctionType a :& ShaderFunctionType b
  mkShaderFunction (a :& b) = mkShaderFunction a :& mkShaderFunction b


type family LinkList (ts :: [*]) where
  LinkList (t1 : t2 : '[]) = t1 :& t2
  LinkList '[t] = t
  LinkList (t1 : ts) = t1 :& LinkList ts
  LinkList '[] = ()


type family MapTypeList (f :: * -> *) (ts :: [*]) where
  MapTypeList f '[] = '[]
  MapTypeList f (x:xs) = f x : MapTypeList f xs


list2LinkList
  :: forall (ts :: [*]) a x (xs :: [*]) y.
     ( Typeable ts
     , Typeable x
     , Typeable xs
     , Typeable (LinkList xs)
     , Typeable y
     , Typeable (LinkList (x : xs))
     )
  => [Value a]
  -> LinkList ts
list2LinkList vs
  | Just (Refl :: '[] :~: ts)  <- eqT = ()
  | Just (Refl :: (x:xs) :~: ts) <- eqT
  , Just (Refl :: LinkList (x : xs) :~: Value y :& LinkList xs) <- eqT
  , y:ys <- vs = (castValue y) :& (list2LinkList @xs ys)


uniformUpdates
  :: forall t ts. ( Shader t
                  , ts ~ MapTypeList Value (MapTypeList Uniform (Uniforms t))
                  , ShaderFunction (LinkList ts)
                  )
  => t
  -> ShaderFunctionType (LinkList ts)
uniformUpdates t = mkShaderFunction $ list2LinkList @ts
                                    $ shaderLinkageUniforms
                                    $ linkages t


attribBuffers
  :: forall t ts. ( Shader t
                  , ts ~ MapTypeList Value (MapTypeList In (Ins t))
                  , ShaderFunction (LinkList ts)
                  )
  => t
  -> ShaderFunctionType (LinkList ts)
attribBuffers t = mkShaderFunction $ list2LinkList @ts
                                   $ shaderLinkageAttribs
                                   $ linkages t

--------------------------------------------------------------------------------
-- Uniform update functions
--------------------------------------------------------------------------------
$(mconcat <$> traverse
  (\(typFrom, typTo, func) ->
    [d|
    instance ShaderFunction (Value (Uniform $typTo)) where
      type ShaderFunctionType (Value (Uniform $typTo)) = GLuint
                                                      -> IO ($typFrom -> IO ())
      mkShaderFunction v program = do
        loc <- withCString (valueToName v) $ glGetUniformLocation program
        return $ \val -> do
          glUseProgram program
          $func loc val
          glGetError >>= \case
            0 -> return ()
            e -> do
              putStrLn $ unwords [ "Could not update uniform"
                                , valueToName v
                                , "with value"
                                , show val
                                , ", encountered error (" ++ show e ++ ")"
                                , show (GL_INVALID_OPERATION :: Integer, "invalid operation" :: String)
                                , show (GL_INVALID_VALUE :: Integer, "invalid value" :: String)
                                ]
              assert False $ return ()

    |]
  )
  [ ( [t|Bool|]
    , [t|Bool|]
    , [| \loc b -> glUniform1i loc $ if b then 1 else 0 |]
    )
  , ( [t|Int|]
    , [t|Int|]
    , [| \loc enum -> glUniform1i loc $ fromIntegral $ fromEnum enum |]
    )
  , ( [t|V2 Int|]
    , [t|Vec 2 Int|]
    , [| \loc v -> let V2 x y = fmap fromIntegral v in glUniform2i loc x y |]
    )
  , ( [t|V3 Int|]
    , [t|Vec 3 Int|]
    , [| \loc v -> let V3 x y z = fmap fromIntegral v in glUniform3i loc x y z |]
    )
  , ( [t|V4 Int|]
    , [t|Vec 4 Int|]
    , [| \loc v -> let V4 x y z w = fmap fromIntegral v in glUniform4i loc x y z w |]
    )
  , ( [t|Float|]
    , [t|Float|]
    , [| \loc -> glUniform1f loc . realToFrac |]
    )
  , ( [t|V2 Float|]
    , [t|Vec 2 Float|]
    , [| \loc v -> let V2 x y = fmap realToFrac v in glUniform2f loc x y |]
    )
  , ( [t|V3 Float|]
    , [t|Vec 3 Float|]
    , [| \loc v -> let V3 x y z = fmap realToFrac v in glUniform3f loc x y z |]
    )
  , ( [t|V4 Float|]
    , [t|Vec 4 Float|]
    , [| \loc v -> let V4 x y z w = fmap realToFrac v in glUniform4f loc x y z w |]
    )
  , ( [t|M44 Float|]
    , [t|Mat 4 4|]
    , [| \loc val -> with val $ glUniformMatrix4fv loc 1 GL_TRUE . castPtr |]
    )
  ]
 )


--------------------------------------------------------------------------------
-- Attribute buffering functions
--------------------------------------------------------------------------------
convertVec
  :: (Unbox (f Float), Foldable f) => Vector (f Float) -> S.Vector GLfloat
convertVec =
  S.convert . V.map realToFrac . V.concatMap (V.fromList . F.toList)


class GLComponentType t where
  compType :: GLenum


instance GLComponentType Float where compType = GL_FLOAT
instance GLComponentType Int where compType = GL_INT


bindAndBuffer :: (Unbox a, Storable a) => GLuint -> Vector a -> IO ()
bindAndBuffer buf as = do
  let asize = V.length as * sizeOf (V.head as)
  glBindBuffer GL_ARRAY_BUFFER buf
  S.unsafeWith (S.convert as) $ \ptr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral asize) (castPtr ptr) GL_STATIC_DRAW


instance ShaderFunction (Value (In Float)) where
  type ShaderFunctionType (Value (In Float)) = GLuint -> GLuint -> Vector Float -> IO ()
  mkShaderFunction _ loc buf as = do
    bindAndBuffer buf as
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc 1 GL_FLOAT GL_FALSE 0 nullPtr
    err <- glGetError
    when (err /= 0) $ do
      print err
      assert False $ return ()


instance ShaderFunction (Value (In Int)) where
  type ShaderFunctionType (Value (In Int)) = GLuint -> GLuint -> Vector Int -> IO ()
  mkShaderFunction _ loc buf as = do
    bindAndBuffer buf as
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc 1 GL_INT GL_FALSE 0 nullPtr
    err <- glGetError
    when (err /= 0) $ do
      print err
      assert False $ return ()


type family ToLinear t where
  ToLinear (Vec 2 t) = V2 (ToLinear t)
  ToLinear (Vec 3 t) = V3 (ToLinear t)
  ToLinear (Vec 4 t) = V4 (ToLinear t)
  ToLinear t = t


instance ( Unbox (ToLinear (Vec n t))
         , Storable (ToLinear (Vec n t))
         , GLComponentType t
         , KnownNat n
         ) => ShaderFunction (Value (In (Vec n t))) where
  type ShaderFunctionType (Value (In (Vec n t))) = GLuint -> GLuint -> Vector (ToLinear (Vec n t)) -> IO ()
  mkShaderFunction _ loc buf as = do
    bindAndBuffer buf as
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc
                          (fromIntegral $ natVal $ Proxy @n)
                          (compType @t)
                          GL_FALSE
                          0
                          nullPtr
    err <- glGetError
    when (err /= 0) $ do
      print err
      assert False $ return ()
