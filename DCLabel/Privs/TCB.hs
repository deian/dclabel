{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 704)
{-# LANGUAGE Unsafe #-}
#endif

{-|

This module implements DCLabel privileges. Since privilege objects may
be used unsafely, this module is marked @-XUnsafe@. Untrusted code may
access privileges using the interface provided by "DCLabel.Privs".

-}

module DCLabel.Privs.TCB (
  -- * Privileges
  -- $privs
    DCPrivDesc(..)
  , DCPriv(..), dcPrivTCB
  , allPrivTCB
  ) where

import DCLabel.Core

-- | A privilege description is simply a conjunction of disjunctions.
-- Unlike (actually minted) privileges (see 'DCPriv'), privilege
-- descriptions may be created by untrusted code.
newtype DCPrivDesc = DCPrivDesc { unDCPrivDesc :: Component }
  deriving (Eq, Show, Read)

-- | A privilege is a minted and protected privilege description
-- ('DCPrivDesc') that may only be created by trusted code or
-- delegated from an existing @DCPriv@.
newtype DCPriv = DCPrivTCB { unDCPriv :: DCPrivDesc }
  deriving (Eq, Show, Read)

-- | The all privilege corresponds to logical @False@
allPrivTCB :: DCPriv
allPrivTCB = dcPrivTCB . DCPrivDesc $ dcFalse

-- | Create a new privilege given a description.
dcPrivTCB :: DCPrivDesc -> DCPriv
dcPrivTCB = DCPrivTCB
