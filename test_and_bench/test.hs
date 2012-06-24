{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances
import DCLabel.Core
import DCLabel.Privs
import Data.Set hiding (map)

import Instances


-- Reduction function toLNF does not modify the semantics of the label
prop_dcReduce :: Component -> Bool
prop_dcReduce l = let l' = dcReduce l 
                  in  l `dcImplies` l' && l' `dcImplies` l 

-- Idempotenncy of dcReduce
prop_dcReduce_idem :: Property
prop_dcReduce_idem = forAll (arbitrary :: Gen Component) $ \l->
  let l'  = dcReduce l 
      l'' = dcReduce l' 
  in l' == l''


-- Partial order for DCLabels
prop_dc_porder :: DCLabel -> DCLabel -> Bool
prop_dc_porder l1 l2  = let ge = l1 `canFlowTo` l2
                            le = l2 `canFlowTo` l1
                            eq = l2 == l1
                        in (eq && ge && le) ||  -- ==
                           ((not eq) && (ge || le) && (ge /= le)) || -- < or >
                           (not (eq || ge || le)) -- incomparable

-- L_1 CanFlowTo L_2 ==> L_1 `CanFlowToP p` L_2 for andy p
prop_dc_canFlowToP :: DCLabel -> DCLabel -> Property
prop_dc_canFlowToP l1 l2 = forAll (arbitrary :: Gen DCPriv) $ \p ->
   l1 `canFlowTo` l2 ==> canFlowToP p l1 l2

-- Check that labels flow to their join for DCLabels
prop_DC_join :: DCLabel -> DCLabel -> Bool
prop_DC_join l1 l2  = let l3 = l1 `dcJoin` l2
                          t1 = l1 `canFlowTo` l3
                          t2 = l2 `canFlowTo` l3
                      in t1 && t2

-- Check that join is the least upper bound for DCLabels
-- TODO: we need to fix this since it is difficult to satisfy the
-- hypothesis. 
prop_dc_join_lub ::  DCLabel -> DCLabel -> Property
prop_dc_join_lub l1 l2 = forAll (arbitrary :: Gen DCLabel) $ \l3' ->
 (l1 `canFlowTo` l3') && (l2 `canFlowTo` l3') ==> (l1 `dcJoin` l2) `canFlowTo` l3'
                  

-- Check that meet flows to the labels making it, for DCLabels
prop_dc_meet ::  DCLabel -> DCLabel -> Bool
prop_dc_meet  l1 l2  = let l3 = l1 `dcMeet` l2
                           t1 = l3 `canFlowTo` l1
                           t2 = l3 `canFlowTo` l2
                       in t1 && t2

-- Check that meet the greatest lower bound for DCLabels
prop_dc_meet_glb :: DCLabel -> DCLabel -> Property
prop_dc_meet_glb l1 l2 = forAll (arbitrary :: Gen DCLabel) $ \l3' ->
 (l3' `canFlowTo` l1) && (l3' `canFlowTo` l2) ==> l3' `canFlowTo` (l1 `dcMeet` l2)

-- Check that the top is indeed indeed the highest element in the lattice
prop_dc_top :: DCLabel -> Property
prop_dc_top l1 = forAll (gen l1) $ \l -> l `canFlowTo` dcTop
    where gen :: DCLabel -> Gen DCLabel
          gen _ = arbitrary

-- Check that the bottom is indeed indeed the lowest element in the lattice
prop_dc_bottom :: DCLabel -> Property
prop_dc_bottom _ = forAll (arbitrary :: Gen DCLabel) $ \l -> dcBot `canFlowTo` l

---- LIO's lostar
--lostar :: TCBPriv -> DCLabel -> DCLabel -> DCLabel
--lostar p l g = 
--  let (ls, li) = (toLNF . secrecy $ l, toLNF . integrity $ l)
--      (gs, gi) = (toLNF . secrecy $ g, toLNF . integrity $ g)
--      lp       = toLNF $ priv p
--      rs'      = c2l [c | c <- getCats ls
--                        , not (lp `implies` (c2l [c]))]
--      rs''     = c2l [c | c <- getCats gs
--                        , not (rs' `implies` (c2l [c]))]
--      rs       = if ls == allComponent || gs == allComponent
--                  then allComponent
--                  else rs' `and_component` rs''
--      ri       = (li `and_component` lp) `or_component` gi
-- in toLNF $ simpleNewComponent p (newDC rs ri)
--      where getCats = conj . component
--            c2l = MkComponent . MkConj
--            simpleNewComponent pr lr | pr == rootPrivTCB = g   
--                                     | pr == noPriv      = l `join` g
--                                     | otherwise         = lr
--
--{-
--lr = lostar p li lg satisfies:
--   - canFlowTo lg lr
--   - canFlowTo_p p li lr
--   - lr is the greatest lower bound
---}
--prop_lostar :: TCBPriv -> DCLabel -> DCLabel -> Property
--prop_lostar p li lg = 
--  let lr = lostar p li lg 
--  in forAll (arbitrary :: Gen DCLabel) $ \lr' -> 
--   	canFlowTo lg lr &&
--   	canFlowTo_p p li lr &&
--	not ( canFlowTo lg lr' &&
--              canFlowTo_p p li lr' &&
--	      lr' /= lr &&
--	      canFlowTo lr' lr)
--
---- | Test serialization.
--prop_DC_serialize :: DCLabel -> Bool
--prop_DC_serialize l = case decode (encode l) of
--                        Left _ -> False
--                        Right l' -> l == l'
--
main :: IO ()
main = defaultMain tests
--
tests :: [Test]
tests = [
    testProperty "dcReduce" prop_dcReduce
  , testProperty "Idempotence of function dcReduce"           prop_dcReduce_idem
  , testProperty "Property of top"                            prop_dc_top
  , testProperty "Property of bottom"                         prop_dc_bottom
  , testProperty "Join operation"                             prop_DC_join
  , testProperty "Join operation is the least upper bound"    prop_dc_join_lub
  , testProperty "Meet operation"                             prop_dc_meet
  , testProperty "Meet operation is the greatest lower bound" prop_dc_meet_glb
  , testProperty "DC labels form a partial order"             prop_dc_porder
  , testProperty "Flow check with privs is less restricting"  prop_dc_canFlowToP 
--  , testProperty "lostar implementation"
--                  (prop_lostar :: TCBPriv -> DCLabel -> DCLabel -> Property)
--  , testProperty "Serialization of DC labels"
--              (prop_DC_serialize :: DCLabel -> Bool)
  ]

