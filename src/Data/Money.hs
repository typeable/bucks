{-# LANGUAGE CPP #-}
module Data.Money (module Export) where

import Data.Money.Internal as Export

#ifdef flag_AESON
import Data.Money.JSON as Export ()
#endif
#ifdef flag_REL8
import Data.Money.Rel8 as Export ()
#endif
