{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 704)
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE DeriveDataTypeable #-}

{-|

This module implements the trusted compoenet of DCLabel privileges,
documented in "DCLabel.Privs".
Since privilege objects may be used unsafely, this module is marked
@-XUnsafe@. Untrusted code may access privileges using the interface
provided by "DCLabel.Privs".

-}

module DCLabel.Privs.TCB (
  -- * Privileges
    DCPrivDesc
  , DCPriv(..), dcPrivTCB
  , noPriv, allPrivTCB
  ) where

import Data.Monoid
import Data.Typeable
import DCLabel.Core

-- | A privilege description is simply a conjunction of disjunctions.
-- Unlike (actually minted) privileges (see 'DCPriv'), privilege
-- descriptions may be created by untrusted code.
type DCPrivDesc = Component

-- | A privilege is a minted and protected privilege description
-- ('DCPrivDesc') that may only be created by trusted code or
-- delegated from an existing @DCPriv@.
newtype DCPriv = DCPrivTCB { unDCPriv :: DCPrivDesc }
  deriving (Eq, Show, Typeable)

-- | Privileges can be combined using 'mappend'
instance Monoid DCPriv where
  mempty = noPriv
  mappend p1 p2 = DCPrivTCB . dcReduce $! (unDCPriv p1) `dcAnd` (unDCPriv p2)

-- | The empty privilege, or no privileges, corresponds to logical
-- @True@.
noPriv :: DCPriv
noPriv = DCPrivTCB dcTrue

-- | The all privilege corresponds to logical @False@
allPrivTCB :: DCPriv
allPrivTCB = dcPrivTCB dcFalse

-- | Create a new privilege given a description.
dcPrivTCB :: DCPrivDesc -> DCPriv
dcPrivTCB = DCPrivTCB
