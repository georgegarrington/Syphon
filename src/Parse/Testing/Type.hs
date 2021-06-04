module Parse.Testing.Type where

import Text.Megaparsec

import Test.Hspec.Megaparsec

import AST.Type

import Parse.Type

import Parse.Testing.Root

testTup = ("(a,b)" `testParse` pType) `shouldParse` (mkTuple [ParsedQuant 'a',ParsedQuant 'b'])

list1 = ("[[a]]" `testParse` pType) `shouldParse` (listConstr `TyApp` (listConstr `TyApp` ParsedQuant 'a'))

nested = ("Either (Maybe a) ([a],(b,c))" `testParse` pType) `shouldParse`
	((eitherConstr `TyApp` (maybeConstr `TyApp` ParsedQuant 'a')) `TyApp` (mkTuple [listConstr `TyApp` ParsedQuant 'a',
	mkTuple[ParsedQuant 'b', ParsedQuant 'c']]))

binOpFns = ("")
	--((listConstr `TyApp` listConstr `TyApp` listConstr `TyApp` listConstr) `TyApp` (ParsedQuant 'a'))

{-}
testTup = ("(a,b)" `testParse` pPattern) `shouldParse` (TuplePattern [VarPattern "a",VarPattern "c"])
testSpacedPatterns = ("1:2:_ (Just x)" `testParse` (some pPattern)) 
	`shouldParse` [ConsPattern [IntPattern 1, IntPattern 2, Wildcard], Constructor "Just" [VarPattern "x"]]
testWrongCons = (\str -> str `testParse` pPattern) `shouldFailOn` "1:2:"-}