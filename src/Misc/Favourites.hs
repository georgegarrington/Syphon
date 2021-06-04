module Misc.Favourites where

import Data.Char
import qualified Data.Map as M

import Text.Megaparsec

--I think this is more readable than "elem"
contains :: Eq a => [a] -> a -> Bool
contains xs y = y `elem` xs

--ONLY USE IF YOU KNOWWW FOR A FACT YOU HAVE A JUST!
unJust :: Maybe a -> a
unJust (Just x) = x
unJust _ = error "\n\nERROR! unJust called with a Nothing!"

--drop n elements from the front and end of the list simultaeneously
shave :: Int -> [a] -> [a]
shave i xs = drop i $ take n xs
    where
        n = length xs - i

--drop n elements from the end of the list instead of the front
snip :: Int -> [a] -> [a]
snip n xs = take (length xs - n) xs

takeFromEnd :: Int -> [a] -> [a]
takeFromEnd n xs = drop (length xs - n) xs

--Drop any occurences of y from the list
dropAnyOf :: Eq a => [a] -> a -> [a]
dropAnyOf [] _ = []
dropAnyOf (x:xs) y
    | y == x = dropAnyOf xs y
    | otherwise = x:dropAnyOf xs y

--Check if a list x contains a sublist y
--(So clearly it can be used for strings)
containsSubList :: Eq a => [a] -> [a] -> Bool
containsSubList parentList subList
	| (length parentList) < subLen = False --base case
	| (take subLen parentList) == subList = True
	| otherwise = containsSubList (drop 1 parentList) subList
	where
		subLen = length subList

--Given the error report in string form, do some custom formatting stuff on it
customErrorReport :: String -> String
customErrorReport original = "\n" ++ header ++ "\n" ++ footer ++ "\n\n"
    where
        header = unlines $ take 4 $ lines $ original
        helper ((fst:rest):strs) = (toUpper fst:rest):", ":strs
        --Display as one long string instead of on different lines
        footer = concat $ helper $ drop 4 $ lines $ original

--Given a string and the width of the terminal, format it to be displayed with '-'
--characters around the side. If the width of the terminal is smaller than 
--the length of the string to be displayed, simply return the string
formatTitle :: String -> Int -> String
formatTitle s i
    | i < length s = s
    | otherwise = (replicate lhs '-') ++ borderLHS ++ s 
        ++ borderRHS ++ (replicate rhs '-')
    where 
        --This is the number of characters that are in the center and that we 
        --need to surroung
        borderLHS
            | s `containsSubList` "ERROR" = "! "
            | otherwise = "| "
        borderRHS
            | s `containsSubList` "ERROR" = " !"
            | otherwise = " |"
        middle = length s + 4
        remaining = i - middle
        lhs = remaining `div` 2
        rhs = remaining - lhs
		
prettyErr s = error $ "\n\n" ++ s ++ "\n"

--Cons that might fail and return nothing
failCons :: a -> Maybe [a] -> Maybe [a]
failCons _ Nothing = Nothing
failCons elem (Just es) = Just $ elem:es

--Lookup all keys and return all values, but if one value is missing then return nothing
lookupAll :: Ord a => [a] -> M.Map a b -> Maybe [b]
lookupAll [] _ = Just []
lookupAll (key:keys) map = case M.lookup key map of
	Nothing -> Nothing
	Just val -> val `failCons` (lookupAll keys map)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace