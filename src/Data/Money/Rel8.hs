{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Money.Rel8 () where

import Data.Money.Internal

import Data.Scientific
import Rel8

import Debug.Trace

instance DBType (Money curr) where
  typeInformation =
    let sci = typeInformation @Scientific
    in TypeInformation { encode = encode sci . realToFrac . getAmount
                       , decode = fmap (Money . realToFrac) (decode sci)
                       , typeName = "decimal"
                       }

