module Xast.Utils.List where

import qualified Data.Set as S

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs

allEqual :: Ord a => [a] -> Bool
allEqual xs =
   all
      (uncurry (==))
      (S.cartesianProduct
         (S.fromList xs)
         (S.fromList xs)
      )

anyEqual :: Ord a => [a] -> Bool
anyEqual xs =
   any
      (uncurry (==))
      (S.cartesianProduct
         (S.fromList xs)
         (S.fromList xs)
      )