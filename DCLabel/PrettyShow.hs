{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#endif
{-| This module exports a function 'prettyShow' that pretty prints
'Principal's, 'Clause's, 'Component's and 'DCLabel's.
-}
module DCLabel.PrettyShow (PrettyShow(..), prettyShow) where

import           DCLabel.Core
import           DCLabel.Privs.TCB
import           Text.PrettyPrint
import qualified Data.ByteString.Char8 as S8
import qualified Data.Set as Set



-- | Class used to create a 'Doc' type of DCLabel-related types
class PrettyShow a where
  pShow :: a -> Doc -- ^ Convert to 'Doc'.

-- | Render a 'PrettyShow' type to a string.
prettyShow :: PrettyShow a => a -> String
prettyShow = render . pShow

instance PrettyShow Principal where
  pShow = text . show . S8.unpack. principalName

instance PrettyShow Clause where
  pShow c = let ps = map pShow . Set.toList $! unClause c
            in parens . hsep $! punctuate (text " \\/") ps

instance PrettyShow Component where
  pShow c | isFalse c = text "False"
          | isTrue c  = text "True"
          | otherwise = let cs = map pShow . Set.toList $! unDCFormula c
                        in hsep $! punctuate (text " /\\") cs
                
instance PrettyShow DCLabel where 
  pShow l = let s = dcSecrecy l
                i = dcIntegrity l
            in angle $ pShow s <+> comma <+> pShow i
		where angle txt = (text "<") <> txt <> (text ">")

instance PrettyShow DCPrivDesc where
  pShow = pShow . unDCPrivDesc

instance PrettyShow DCPriv where
  pShow = pShow . unDCPriv
