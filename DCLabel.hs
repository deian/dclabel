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
               , module DCLabel.DSL
               ) where

import DCLabel.Core (
    Principal, principal
  , Clause, clause
  , Component, dcTrue, dcFalse, dcFormula
  , isTrue, isFalse
  , DCLabel, dcLabel
  , dcBot, dcTop, dcPub
  , canFlowTo, dcJoin, dcMeet
  )
import DCLabel.Privs
import DCLabel.DSL
import DCLabel.Serialize ()
