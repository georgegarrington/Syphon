module AST.SUnit where

import AST.Expression
import AST.Pattern

--The module being tested from within the same directory 
--and the test cases for the functions being tested from the module

--For now leave out type checking stuff from the sunit module,
--however it should not be difficult to add this later 
type SUnitMod = [TestDfn]

data TestDfn 
	= Test {name :: String, testCases :: [TestCase]} 
	| GlobalLet Pattern Expr deriving (Show, Eq)

data TestCase 
	= TestCase {
	prettyArgs :: String, 
	prettyPtrn :: String,
	exprs :: [Expr], --The function being tested applied to each of its arguments
	expected :: TestingFor --Either the expected error message or the expected pattern 
	} | LocalLet Pattern Expr deriving (Show, Eq)

data TestingFor = Failure String | ReturnPtrn Pattern | Predicate Expr deriving (Show, Eq)