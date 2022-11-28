{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Money.JSON () where

import Control.Monad (when)
import Data.Aeson
import Data.Money.Internal
import Data.Aeson.Types (typeMismatch)

instance KnownCurrency c => FromJSON (Money c) where
  parseJSON (Object v) = do
    curr <- v .: "currency"
    let expectedCurr = currencyCode @c
    when (curr /= expectedCurr) $
      fail ("Currency code doesn't match: expected " <> expectedCurr <> ", received " <> curr)
    amount <- v .: "amount"
    pure (Money amount)
  parseJSON v = typeMismatch ("Money " <> currencyCode @c) v

instance KnownCurrency c => ToJSON (Money c) where
  toJSON (Money amount) = object [ "currency" .= currencyCode @c
                                 , "amount" .= amount
                                 ]
