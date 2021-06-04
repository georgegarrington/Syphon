{-# LANGUAGE FlexibleInstances #-}
module AST.Expression where

import AST.Pattern
import AST.Type

data Expr
	= BoolVal Bool 
	| IntVal Int
	| DoubleVal Double
	| CharVal Char
	| StringVal String
	| Var String
	| OptVar String Expr --Variable with an optional structure
	| Optional [(String, Expr)]
	| Let Pattern Expr Expr --CHANGE TO SUPPORT PATTERN INSTEAD OF STRING VARIABLE
	| Tuple [Expr]
	| App Expr Expr
	| Application Expr [Expr] --Curried application, keep the parsed format as this and keep the proper application above for type system
	| Lambda [Pattern] Expr --A lambda expression is a function expression in itself that can be applied to argument expressions
	| Match Expr [(Pattern, Expr)]
	| Conditional [(Expr, Expr)]
	| IfEl Expr Expr Expr
	| PatternCheck Expr Pattern
	| RecordConstr String [(String, Expr)] --Construct a record with all its fields
	| FieldReplace String [(String, Expr)] --Make a new record by replacing fields of an existing record
	| FieldSelect Expr String --Select a field from a record expression e.g. state.count, (extractState someExpr).field1
	| ListGen Expr (Maybe Expr) Expr --E.g. from 1 to 2, from x to y, maybe an interval or maybe not
	--Calls the runtime error function and makes the function that called it return undefined
	--boolean for whether it is a dialog message or not, if not then it is a console message
	| Error Expr 
	deriving (Eq, Show)

instance Show (Expr -> Expr) where
	show e = "Partially applied expression"

--Returns a nested let expression that takes one more body expression (the case expression) before
--finally becoming an expression itself
nestLets :: [Expr -> Expr] -> (Expr -> Expr)
nestLets [l] = l 
nestLets [l1,l2] = \body -> l1 (l2 body)
nestLets (l:ls) = \body -> l (nestLets ls $ body)