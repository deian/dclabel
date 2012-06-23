
module DCLabel.Privs (
  -- * Privileges
  -- $privs
    DCPrivDesc(..)
  , DCPriv
  , noPriv
  , dcDelegatePriv
  , dcOwns
  , canFlowToP
  ) where

import DCLabel.Core
import DCLabel.Privs.TCB
import qualified Data.Set as Set


-- | The empty privilege, or no privileges, corresponds to logical
-- @True@.
noPriv :: DCPriv
noPriv = DCPrivTCB . DCPrivDesc $! dcTrue


-- | Given a privilege and a privilege description turn the privilege
-- description into a privilege (i.e., mint). Such delegation succeeds
-- only if the supplied privilege implies the privilege description.
dcDelegatePriv :: DCPriv -> DCPrivDesc -> Maybe DCPriv
dcDelegatePriv p pd = let c  = unDCPrivDesc . unDCPriv $! p
                          cd = unDCPrivDesc pd
                      in if c `dcImplies` cd
                           then Just $! dcPrivTCB pd
                           else Nothing

-- | We say a piece of code having a privilege object (of type
-- 'DCPriv') owns a clause when the privileges allow code to bypass
-- restrictions imposed by the clause. This is the case if and only if
-- the 'DCPriv' object contains one of the 'Principal's in the 'Clause'.
-- This function can be used to make such checks.
dcOwns :: DCPrivDesc -> Clause -> Bool
dcOwns pd c = unDCPrivDesc pd `dcImplies` dcFormula (Set.singleton c)


class CanFlowTo p where
  -- | Can flow to relation given a set of privileges.
  canFlowToP :: p -> DCLabel -> DCLabel -> Bool

instance CanFlowTo DCPrivDesc where
  canFlowToP pd l1 l2 =
    let cp = unDCPrivDesc pd
        i1 = dcReduce $ dcIntegrity l1 `dcAnd` cp
        s2 = dcReduce $ dcSecrecy l2   `dcAnd` cp
    in l1 { dcIntegrity = i1 } `canFlowTo` l2 { dcSecrecy = s2 }

instance CanFlowTo DCPriv where
  canFlowToP p = canFlowToP (unDCPriv p)
