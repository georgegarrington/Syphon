module Parse.Testing.Pattern where

import Text.Megaparsec

import Test.Hspec.Megaparsec

import AST.Pattern

import Parse.Pattern

import Parse.Testing.Root

testTup = ("(a,b)" `testParse` pPattern) `shouldParse` (TuplePattern [VarPattern "a",VarPattern "c"])
testSpacedPatterns = ("1:2:_ (Just x)" `testParse` (some pPattern)) 
	`shouldParse` [ConsPattern [IntPattern 1, IntPattern 2, Wildcard], Constructor "Just" [VarPattern "x"]]
testWrongCons = (\str -> str `testParse` pPattern) `shouldFailOn` "1:2:"