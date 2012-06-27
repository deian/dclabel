{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
{- | 

This module re-export the core /DCLabel/ interface. For a description
of DCLabels see "DCLabel.Core".

-}

module DCLabel ( module DCLabel.Core
               , module DCLabel.Privs
               , module DCLabel.NanoEDSL
               , module DCLabel.Serialize
               , module DCLabel.PrettyShow
               ) where

import DCLabel.Core
import DCLabel.Privs
import DCLabel.NanoEDSL
import DCLabel.Serialize
import DCLabel.PrettyShow
