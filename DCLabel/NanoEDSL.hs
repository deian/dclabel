{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  This module implements a ``nano``, very simple, embedded domain
  specific language to create 'Component's and privilage descriptions
  from conjunctions of principal disjunctions.
  
  A 'Component'/'DCPrivDesc' is created using the ('\/') and ('/\') operators.
  The disjunction operator ('\/') is used to create a 'Clause' from
  'Principal's, ByteStrings, or a disjunctive sub-expression. For example:

  @
     p1 = 'principal' \"p1\"
     p2 = 'principal' \"p2\"
     p3 = 'principal' \"p3\"
     e1 = p1 '\/' p2
     e2 = e1 '\/' \"p4\"
  @

  Similarly, the conjunction operator ('/\') is used to create category-sets
  from 'Principal's, ByteStrings, and conjunctive or disjunctive sub-expressions.
  For example:

  @
     e3 = p1 '\/' p2
     e4 = e1 '/\' \"p4\" '/\' p3
  @

  /Note/ that because a clause consists of a disjunction of principals, and a
  component is composed of the conjunction of categories, ('\/') binds
  more tightly than ('/\').

  Given two 'Component's, one for secrecy and one for integrity, you
  can create a 'DCLabel' with 'dcLabel'. Given a 'Component' you can
  create a 'DCPrivDesc' using 'dcPrivDesc'. Finally, given a 'DCPriv'
  and 'DCPrivDesc' you can create a new minted privilege with
  'dcDelegatePriv'.
  
  
  Consider the following, example:

  @
     l1 = (\"Alice\" :: 'Principal')  '\/' (\"Bob\"  :: 'Principal') '/\' (\"Carla\"  :: 'Principal') 
     l2 = (\"Alice\" :: 'Principal') '/\' (\"Carla\"  :: 'Principal') 
     dc1 = 'dcLabel' l1 l2
     dc2 = 'dcLabel' ('toComponent' (\"Djon\" :: 'Principal')) ('toComponent' (\"Alice\" :: 'Principal'))
     pr = 'dcPrivTCB' . 'dcPrivDesc' $ (\"Alice\" :: 'Principal') '/\' (\"Carla\" :: 'Principal')
  @

where

  * @ dc1 = \<{[\"Alice\" &#8897; \"Bob\"] &#8896; [\"Carla\"]} , {[\"Alice\"] &#8896; [\"Carla\"]}\>@
  
  * @ dc2 = \<{[\"Djon\"]} , {[\"Alice\"]}\>@

  * @ 'canFlowTo' dc1 dc2 = False @

  * @ 'canFlowToP' pr dc1 dc2 = True@

-}

module DCLabel.NanoEDSL ( -- * Operators
			  (\/), (/\), toComponent
                          -- * Aliases
                        , everybody, anybody
                        ) where

import DCLabel.Core
import qualified Data.Set as Set

-- | Convert a type (e.g., 'Clause', 'Principal') to a label component.
class ToComponent a where
  -- | Convert to 'Component'
  toComponent :: a -> Component

instance ToComponent Component where
  toComponent = id
instance ToComponent Clause    where
  toComponent c = DCFormula $! Set.singleton c
instance ToComponent Principal where
  toComponent p = toComponent . Clause $! Set.singleton p

infixl 7 \/
infixl 6 /\\

-- | Conjunction of two 'Principal'-based elements.
(/\) :: (ToComponent a, ToComponent b) => a -> b -> Component
a /\ b = dcReduce $! toComponent a `dcAnd` toComponent b

-- | Disjunction of two 'Principal'-based elements.
(\/) :: (ToComponent a, ToComponent b) => a -> b -> Component
a \/ b = dcReduce $! toComponent a `dcOr` toComponent b

--
-- Aliases
--

-- | Logical falsehood can be thought of as the component containing
-- every possible principal:
--
-- > everybody = dcFalse
--
everybody :: Component
everybody = dcFalse

-- | Logical truth can be thought of as the component containing
-- no specific principal:
--
-- > anybody = dcTrue
--
anybody :: Component
anybody = dcTrue
