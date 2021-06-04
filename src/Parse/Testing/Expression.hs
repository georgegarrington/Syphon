module Parse.Testing.Expression where

import Text.Megaparsec

import Test.Hspec.Megaparsec

import AST.Expression
import AST.Util

import Parse.Expression

import Parse.Testing.Root

--Test to see whether or not binary operators affect function application, we can see that they don't
list1 = ("fn0 arg00 arg01 + fn1 arg10 arg11 + fn2 arg20 arg21" `testParse` pExpr)
	`shouldParse` App (App (Var "+") (App (App (Var "+") firstPart) secondPart)) thirdPart
	where
		firstPart = applicationify [Var "fn0", Var "arg00", Var "arg01"]
		secondPart = applicationify [Var "fn1", Var "arg10", Var "arg11"]
		thirdPart = applicationify [Var "fn2", Var "arg20", Var "arg21"]