{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Money.Internal ( Money(..)
                           , Currency
                           , KnownCurrency(..)
                           ) where

import Data.Fixed (Centi)
import Data.Group
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

-- | Money type. This is actually 'Centi' from 'Data.Fixed' in disguise.
--   The type parameter is phantom, you probably want to use 'Currency' there.
--   See also 'Data.Money.Currencies' for currency types.
newtype Money curr = Money { getAmount :: Centi }
  deriving (Eq, Ord, Generic)

-- | Empty type, used only to carry its type parameter with a currency code around. E.g. 'Currency "USD"', 'Currency "GBP"', et cetera.
--   See Data.Money.Currencies for pre-defined currency types.
data Currency (name :: Symbol)

-- | Statically known currency type class.
class KnownCurrency c where
  currencyCode :: String

instance KnownSymbol c => KnownCurrency (Currency c) where
  currencyCode = symbolVal (Proxy @c)

instance Semigroup (Money curr) where
  Money a <> Money b = Money (a + b)

instance Monoid (Money curr) where
  mempty = Money 0

instance Group (Money curr) where
  invert (Money m) = Money (-m)
  Money m ~~ Money m' = Money (m - m')
  pow (Money m) n = Money (m * fromIntegral n)

instance Abelian (Money curr)

instance KnownCurrency c => Show (Money c) where
  show (Money x) = currencyCode @c <> " " <> show x

instance KnownCurrency c => Read (Money c) where
  readsPrec p str
    | expCurr == take l str
    , [(am, rest)] <- readsPrec p (drop l str) = [(Money am, rest)]
    | otherwise = []
    where expCurr = currencyCode @c <> " "
          l = length expCurr
