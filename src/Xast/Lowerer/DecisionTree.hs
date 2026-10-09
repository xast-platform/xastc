module Xast.Lowerer.DecisionTree where

import Xast.AST (Ident)
import Xast.Utils.Generic ((|>), unreachableWith)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import qualified Data.Set as S

data DecisionTree
   = DTLeaf Int
   | DTFail
   | DTSwitch [(Con, DecisionTree)] (Maybe DecisionTree)
   | DTSwap Int DecisionTree

compilePatterns :: PatMatrix -> DecisionTree
compilePatterns pm
   | null pm = DTFail
   | pm 
      |> head 
      |> rowContainsWildcardOnly = 
         let action = pm |> head |> rowAction
         in DTLeaf action

   | otherwise =
      let cols = head (patMatrixColsWithCon pm)
          i =  cols
      in if i == 0 then
         let headCons = patMatrixHeadCons pm
             def = 
               if null headCons then
                  Just (compilePatterns pm)
               else
                  Nothing
             caseList = headCons
               |> map (\con -> 
                     let pmSpecialized = specializeS con pm
                     in (con, compilePatterns pmSpecialized)
                  )
         in DTSwitch caseList def
      else
         let pmSwapped = patMatrixSwap i pm
         in DTSwap i (compilePatterns pmSwapped)

data Con = Con
   { name  :: Ident
   , arity :: Int
   , span  :: Int
   } deriving (Eq, Ord, Show)

data Pat
   = PWildCard
   | PCon Con [Pat]
   deriving (Eq, Ord, Show)

patCon :: Pat -> Maybe Con
patCon PWildCard = Nothing
patCon (PCon con _) = Just con

isCon :: Pat -> Bool
isCon = isJust . patCon

data Row = Row [Pat] Int

rowAction :: Row -> Int
rowAction (Row _ a) = a

rowContainsWildcardOnly :: Row -> Bool
rowContainsWildcardOnly (Row pats _) = pats
   |> filter (/= PWildCard)
   |> null

rowHeadIs :: Con -> Row -> Bool
rowHeadIs conA (Row (PCon conB _ : _) _) = conA == conB
rowHeadIs _ _ = False

rowHeadIsCon :: Row -> Bool
rowHeadIsCon (Row (PCon _ _ : _) _) = True
rowHeadIsCon _ = False

rowHeadIsWildcard :: Row -> Bool
rowHeadIsWildcard (Row (PWildCard : _) _) = True
rowHeadIsWildcard _ = False

type PatMatrix = [Row]

patMatrixSwap :: Int -> PatMatrix -> PatMatrix
patMatrixSwap index = map swapRow
   where
      swapRow :: Row -> Row
      swapRow (Row pats n) = Row (swapElements 0 index pats) n

      swapElements :: Int -> Int -> [a] -> [a]
      swapElements i j xs
         | i == j = xs
         | i < 0 || j < 0 = xs
         | i >= length xs || j >= length xs = xs
         | otherwise =
            let a = xs !! i
                b = xs !! j
            in replace i b (replace j a xs)

      replace :: Int -> a -> [a] -> [a]
      replace _ _ [] = []
      replace 0 x (_:xs) = x : xs
      replace i x (y:ys) = y : replace (i - 1) x ys

patMatrixGetSig :: Int -> PatMatrix -> [Con]
patMatrixGetSig index pm = pm
   |> map (\(Row pats _) -> pats !! index |> patCon)
   |> catMaybes
   |> S.fromList
   |> S.toList

patMatrixColsWithCon :: PatMatrix -> [Int]
patMatrixColsWithCon pm = pm
   |> concatMap (\(Row pats _) ->
         pats
            |> zip [0..]
            |> filter (isCon . snd)
            |> map fst
      )
   |> S.fromList
   |> S.toList

patMatrixHeadCons :: PatMatrix -> [Con]
patMatrixHeadCons pm = pm
   |> map (\(Row pats _) -> pats |> head |> patCon)
   |> catMaybes
   |> S.fromList
   |> S.toList

specializeS :: Con -> PatMatrix -> PatMatrix
specializeS con = mapMaybe (specializeRow con)

specializeRow :: Con -> Row -> Maybe Row
specializeRow con row@(Row pats0 index)
  | rowHeadIs con row = 
      let rowHead = head pats0
          pats1 = case rowHead of
            PCon _ args -> args
            _ -> unreachableWith "head must be a constructor"
          pats2 = pats1 ++ tail pats0

      in Just (Row pats2 index)

  | rowHeadIsWildcard row = 
      let pats1 = replicate (length pats0) PWildCard
          pats2 = pats1 ++ tail pats0
      in Just (Row pats2 index)
      
  | otherwise = Nothing

defaultD :: PatMatrix -> PatMatrix
defaultD = mapMaybe defaultRow

defaultRow :: Row -> Maybe Row
defaultRow row@(Row pats0 index)
   | rowHeadIsWildcard row = Just (Row (tail pats0) index)
   | otherwise = Nothing
