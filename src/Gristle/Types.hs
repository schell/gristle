{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gristle.Types where

import           Data.Fix       (Fix (..))
import           Data.Ratio     (denominator, numerator)

import           Gristle.Syntax
import           Gristle.TH

-- $setup
-- >>> :set -XTypeApplications

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
--
-- | Values can be expressed with fractions!
--
-- >>> gPrint $ floatVal 20 / floatVal 6
-- (20.0 / 6.0)
--
-- | Values can be expressed with floating point operations:
--
-- >>> gPrint $ pi @(Value Float)
-- 3.1415927
--
-- >>> gPrint $ floatVal 2 ** floatVal 4
-- pow(2.0, 4.0)


$(genStuff genNumInstance [ (t, c)
                          | t <- floatingBaseTypes
                          , c <- pure floatingFromIntegral
                          ]
 )

$(genStuff genFractionalInstance [ (t, c)
                                 | t <- floatingBaseTypes
                                 , c <- pure $ constructFromRational [e| litVal |]
                                 ]
 )

$(genStuff genFloatingInstance [ (t, c)
                               | t <- floatingBaseTypes
                               , c <- pure [e| Value $ Fix $ Literal $ LitFloat pi |]
                               ]
 )

$(genStuff genNumInstance $ [ (t, c)
                            | t <- integralBaseTypes
                            , c <- pure [e| litVal . fromIntegral |]
                            ]
 )
