{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
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
import           Data.Type.Equality    hiding (apply)
import           Data.Typeable         (Typeable, eqT)
import qualified Data.Vector.Storable  as S
import           Data.Vector.Unboxed   (Unbox, Vector)
import qualified Data.Vector.Unboxed   as V
import           Foreign.C.String      (withCString)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr           (castPtr, nullPtr)
import           Foreign.Storable      (Storable (..))
import           GHC.TypeLits          (KnownNat, natVal)
import           Graphics.GL
import           Linear

import           Gristle
import           Gristle.GLSL
import           Gristle.Syntax
import           Gristle.Vector

--------------------------------------------------------------------------------
-- Uniform update class
--------------------------------------------------------------------------------


class HasUniformUpdates t where
  type UniformUpdates t
  toUniformUpdates :: t -> UniformUpdates t


instance ( HasUniformUpdates a
         , HasUniformUpdates b
         ) => HasUniformUpdates (a :& b) where
  type UniformUpdates (a :& b) = UniformUpdates a :& UniformUpdates b
  toUniformUpdates (a :& b) = toUniformUpdates a :& toUniformUpdates b


instance HasUniformUpdates () where
  type UniformUpdates () = ()
  toUniformUpdates () = ()


uniformUpdates
  :: ( Shader t
     , HasUniformUpdates (Uniforms t)
     )
  => t -> UniformUpdates (Uniforms t)
uniformUpdates = toUniformUpdates . shaderLinkageUniforms . linkages


--------------------------------------------------------------------------------
-- Uniform update functions
--------------------------------------------------------------------------------
$(mconcat <$> traverse
  (\(typFrom, typTo, func) ->
    [d|
    instance HasUniformUpdates (Value $typTo) where
      type UniformUpdates (Value $typTo) =
        (GLuint -> IO ($typFrom -> IO ()))
      toUniformUpdates v program = do
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


class HasBufferableAttribs t where
  type BufferableAttribs t
  toBufferableAttribs :: t -> BufferableAttribs t


instance ( HasBufferableAttribs a
         , HasBufferableAttribs b
         ) => HasBufferableAttribs (a :& b) where
  type BufferableAttribs (a :& b) = BufferableAttribs a :& BufferableAttribs b
  toBufferableAttribs (a :& b) = toBufferableAttribs a :& toBufferableAttribs b


instance HasBufferableAttribs () where
  type BufferableAttribs () = ()
  toBufferableAttribs () = ()


attribBuffers
  :: ( Shader t
     , HasBufferableAttribs (Ins t)
     )
  => t -> BufferableAttribs (Ins t)
attribBuffers = toBufferableAttribs . shaderLinkageAttribs . linkages


--------------------------------------------------------------------------------
-- Attribute buffering functions
--------------------------------------------------------------------------------
clearErrors :: String -> IO ()
clearErrors str = do
    err' <- glGetError
    when (err' /= 0) $ do
      putStrLn $ unwords [str, show err']
      assert False $ return ()


withVAO :: (GLuint -> IO b) -> IO b
withVAO f = do
    [vao] <- allocaArray 1 $ \ptr -> do
        glGenVertexArrays 1 ptr
        peekArray 1 ptr
    glBindVertexArray vao
    r <- f vao
    clearErrors "withVAO"
    glBindVertexArray 0
    return r


withBuffers :: Int -> ([GLuint] -> IO b) -> IO b
withBuffers n f = do
    bufs <- allocaArray n $ \ptr -> do
        glGenBuffers (fromIntegral n) ptr
        peekArray (fromIntegral n) ptr
    f bufs


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


instance HasBufferableAttribs (Value Float) where
  type BufferableAttribs (Value Float) =
    GLuint -> IO (GLuint -> Vector Float -> IO ())
  toBufferableAttribs val program = do
    iloc <- withCString (valueToName val) $ glGetAttribLocation program
    return $ \buf as -> do
      let loc = fromIntegral iloc
      bindAndBuffer buf as
      glEnableVertexAttribArray loc
      glVertexAttribPointer loc 1 GL_FLOAT GL_FALSE 0 nullPtr
      err <- glGetError
      when (err /= 0) $ do
        print err
        assert False $ return ()


instance HasBufferableAttribs (Value Int) where
  type BufferableAttribs (Value Int) =
    GLuint -> IO (GLuint -> Vector Int -> IO ())
  toBufferableAttribs val program = do
    iloc <- withCString (valueToName val) $ glGetAttribLocation program
    return $ \buf as -> do
      let loc = fromIntegral iloc
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
         ) => HasBufferableAttribs (Value (Vec n t)) where
  type BufferableAttribs (Value (Vec n t)) =
    GLuint -> IO (GLuint -> Vector (ToLinear (Vec n t)) -> IO ())
  toBufferableAttribs val program = do
    iloc <- withCString (valueToName val) $ glGetAttribLocation program
    return $ \buf as -> do
      let loc = fromIntegral iloc
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

drawBuffer :: GLuint
           -> GLuint
           -> GLenum
           -> GLsizei
           -> IO ()
drawBuffer program vao mode num = do
    glUseProgram program
    glBindVertexArray vao
    clearErrors "drawBuffer:glBindVertex"
    glDrawArrays mode 0 num
    clearErrors "drawBuffer:glDrawArrays"
