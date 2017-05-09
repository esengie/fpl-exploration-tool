{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen.Infer.Solver(
  ctxAddLtoR,
  ctxTrimLtoR,
  Adder(..),
  Remover(..),
  Swapper(..)
) where

import Data.List (elemIndex, elem)
import Data.Maybe (fromJust)

import AST(Ctx)

newtype Adder = A Int
  deriving (Show, Eq, Num)

newtype Swapper = Sw (Int, Int)
  deriving (Show, Eq)

newtype Remover = R Int
  deriving (Show, Eq, Num)

-- this will be minimal as anything in number of funcs generated
-- number of swaps - well you could quicksort, but feck it
-- Prem: list are equal as sets, no dups
-- swapLtoR "abcdef" "cbaefd" = [Sw (1,3),Sw (4,5),Sw (5,6)]
swapLtoR :: Eq a => [a] -> [a] -> [Swapper]
swapLtoR l r = helper l r 1
  where
    helper [] [] _ = []
    helper (x:xs) (y:ys) n
      | x == y = helper xs ys (n+1)
      | otherwise =
        let j = elemN y (x:xs)
        in  Sw (n, n + j) : helper ys (drop 1 $ swapElementsAt 0 j (x:xs)) (n+1)

-- Prem: r is sublist of l, remove all that are not in r
-- remLtoR "axyvrze" "xz" == [R 1,R 2,R 2,R 2,R 3]
remLtoR :: Eq a => [a] -> [a] -> [Remover]
remLtoR [] _ = []
remLtoR (x:xs) ys
  | x `elem` ys = map (+1) (remLtoR xs ys)
  | otherwise = R 1 : remLtoR xs ys


-- Prem: l is sublist of r && shortenLtoR r l == l
--       => addLtoR l r = r, addLtoR = inv(remLtoR)
-- addLtoR "xz" "axyvrze" == [A 3,A 2,A 2,A 2,A 1]
addLtoR :: Eq a => [a] -> [a] -> [Adder]
addLtoR l r = reverse (helper l r 1)
  where
    helper _ [] _ = []
    helper [] (y:ys) n = A n : helper [] ys n
    helper (x:xs) (y:ys) n
      | x == y = helper xs ys (n + 1)
      | otherwise = A n : helper (x:xs) ys n

-- Prem: l > r (in a sense of sets), out: l==r in a sense of sets
shortenLtoR :: Eq a => [a] -> [a] -> [a]
shortenLtoR [] _ = []
shortenLtoR _ [] = []
shortenLtoR (x:xs) ys
  | x `elem` ys = x : shortenLtoR xs ys
  | otherwise = shortenLtoR xs ys

-- given ctx l we want it to be like r
--  Prem: r in l, out: sequence of removes and then swaps to get us r
-- ctxRemLtoR ["x", "z", "y"] ["x", "y"] == ([R 2],[])
-- ctxRemLtoR ["y", "b", "z", "x"] ["z", "x", "y"] == ([R 3],[Sw(1,3), Sw(2,3)])
ctxRemLtoR :: Ctx -> Ctx -> ([Remover], [Swapper])
ctxRemLtoR l r = (removed, swapped)
  where
    (l',r') = (reverse l, reverse r)
    removed  = remLtoR l' r'
    shortL = shortenLtoR l' r'
    swapped = swapLtoR shortL r'

-- in put app we only need to trim
-- ctxTrimLtoR ["y", "b", "z", "x"] ["z", "x", "y"] = (["y", "z", "x"], [R 3])
ctxTrimLtoR :: Ctx -> Ctx -> (Ctx, [Remover])
ctxTrimLtoR l r = (reverse newct, removed)
  where
    (l',r') = (reverse l, reverse r)
    removed  = remLtoR l' r'
    newct = shortenLtoR l' r'

-- given ctx l we want it to be like r
--  Prem: l in r, out: sequence of swaps then adds to get us r
-- ctxAddLtoR ["x", "y"] ["x", "z", "y"] == ([],[A 2])
-- ctxAddLtoR ["z", "x", "y"] ["y", "b", "z", "x"] == ([Sw(1,2), Sw(2,3)], [A 3])
ctxAddLtoR :: Ctx -> Ctx -> ([Swapper], [Adder])
ctxAddLtoR l r = (swapped, added)
  where
    (l',r') = (reverse l, reverse r)
    shortR = shortenLtoR r' l'
    swapped = swapLtoR l' shortR
    added  = addLtoR shortR r'

-- PS this is solved both ways cause I've noticed the inverseness only after completion
-- (you can replace additions with reverse(removals) & swaps with their inverses)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs =
  let elemI = xs !! i
      elemJ = xs !! j
      left = take i xs
      middle = take (j - i - 1) (drop (i + 1) xs)
      right = drop (j + 1) xs
  in  left ++ [elemJ] ++ middle ++ [elemI] ++ right

elemN :: Eq a => a -> [a] -> Int
elemN x xs = fromJust (elemIndex x xs)

incSwap :: Swapper -> Swapper
incSwap (Sw (a, b)) = Sw (a+1, b+1)







---
