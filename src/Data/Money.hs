{-# LANGUAGE CPP #-}
module Data.Money (module Export) where

import Data.Money.Internal as Export

#ifdef flag_AESON
import Data.Money.JSON as Export ()
#endif
