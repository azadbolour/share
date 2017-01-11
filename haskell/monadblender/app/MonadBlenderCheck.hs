module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Test.QuickCheck
import MonadBlender

my_prop :: Int -> Bool
my_prop i = i == i

maybeLiftList :: [Int] -> MaybeT [] Int
maybeLiftList list = lift list
maybeHLiftList :: [Int] -> MaybeT [] Int
maybeHLiftList list = hLift list

-- blending_prop :: [Int] -> Property
-- blending_prop list = let lifted = maybeLiftList list in (collect lifted) $ lifted == maybeHLiftList list

blending_prop :: [Int] -> Bool
blending_prop list = maybeLiftList list == maybeHLiftList list

main :: IO ()

main = quickCheck blending_prop