{- | 

This module re-export the core /DCLabel/ interface. For a description
of DCLabels see "DCLabel.Core".

-}

module DCLabel ( module DCLabel.Core
               , module DCLabel.Privs
               , module DCLabel.NanoEDSL
               ) where

import DCLabel.Core
import DCLabel.Privs
import DCLabel.NanoEDSL
import DCLabel.Serialize
