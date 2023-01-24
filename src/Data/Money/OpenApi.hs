{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Money.OpenApi where

import Data.Fixed
import Data.Money.Internal
import Data.OpenApi
import Data.Typeable

instance (KnownCurrency c, Typeable (Money c)) => ToSchema (Money c) where
  declareNamedSchema _ = do
    centiSchema <- declareSchemaRef (Proxy :: Proxy Centi)
    strSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $ NamedSchema (Just "Money") $
      mempty { _schemaType = Just OpenApiObject
             , _schemaProperties = [ ("currency", strSchema)
                                   , ("amount", centiSchema) ]
             , _schemaRequired = [ "currency", "amount" ]
             }
