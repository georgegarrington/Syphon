module CLI.Util where

import Data.Char

import System.Console.Terminal.Size

import Misc.Favourites hiding (formatTitle)

--The CLI will have a similar discipline to transpiling, each message is responsible for putting a new line after itself

printTitle :: String -> IO ()
printTitle str = do
  tWidth <- fmap (width . unJust) size
  putStrLn $ formatTitle (map toUpper str) tWidth ++ "\n"
	where
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