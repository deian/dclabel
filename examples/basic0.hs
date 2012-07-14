module Main where 

import DCLabel
import DCLabel.Privs.TCB (dcPrivTCB)

-- | Simple secrecy component example
s :: Component
s =  "Alice" \/ "Bob" /\  "Carla"

-- | Simple integrity component example
i :: Component
i = "Alice" /\ "Carla"

-- | Simple label
l1 :: DCLabel
l1 = dcLabel s i

-- | Simple label
l2 :: DCLabel
l2 = dcLabel (toComponent "Djon") (toComponent "Alice")

-- | Creating privilege using constructor from TCB
p :: DCPriv
p = dcPrivTCB  $ "Alice" /\ "Carla"

main = do
  putStrLn $ "Label 1: " ++ show l1
  putStrLn $ "Label 2: " ++ show l2
  putStrLn $ "Join of labels: " ++ show (l1 `dcJoin` l2)
  putStrLn $ "Meet of labels: " ++ show (l1 `dcMeet` l2)
  putStrLn $ "Privileges: " ++ show p
  putStrLn $ "Label 1 flows to Label 2? " ++ (show $ canFlowTo l1 l2)
  putStrLn $ "Label 1 flows to Label 2 given privileges? " ++
             (show $ canFlowToP p l1 l2)
{-
Output:
ghci> main
Label 1: < {[Alice \/ Bob] /\ [Carla]} , {[Alice] /\ [Carla]} >
Label 2: < {[Djon]} , {[Alice]} >
Join of labels: < {[Alice \/ Bob] /\ [Carla] /\ [Djon]} , {[Alice]} >
Meet of labels: < {[Alice \/ Bob \/ Djon] /\ [Carla \/ Djon]} ,
{[Alice] /\ [Carla]} >
Privileges: DCPrivTCB {unDCPriv = {[Alice] /\ [Carla]}}
Label 1 flows to Label 2? False
Label 1 flows to Label 2 given privileges? True
-}
