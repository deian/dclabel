
module Instances where

import Control.Monad (liftM)
import Test.QuickCheck
import Test.QuickCheck.Instances
import DCLabel.Core
import DCLabel.Privs.TCB
import Data.Set hiding (map)

instance Arbitrary Principal where
  arbitrary = Principal `liftM` arbitrary
  --arbitrary = Principal `liftM` (oneof $ map return ["A", "B", "C"])

instance Arbitrary Clause where
  arbitrary = Clause `liftM` arbitrary

instance Arbitrary Component where
  arbitrary = oneof [ return DCFalse
                    , do cs <- arbitrary
                         return . DCFormula $ if (Clause empty) `member` cs
                                                then empty
                                                else cs
                    ]

instance Arbitrary DCLabel where
  arbitrary = do
    s <- dcReduce `liftM` arbitrary
    i <- dcReduce `liftM` arbitrary
    return (dcLabel s i)

instance Arbitrary DCPrivDesc where
  arbitrary = DCPrivDesc `liftM` arbitrary

instance Arbitrary DCPriv where
  arbitrary = DCPrivTCB `liftM` arbitrary
