module AST.Pattern where

data Pattern
	= Wildcard
	| BoolPattern Bool
	| IntPattern Int
	| DoublePattern Double
	| CharPattern Char
	| StringPattern String
	| VarPattern String
	--The last pattern in the cons should be a list type
	| ConsPattern [Pattern]
	| SugarList [Pattern]
	--Keep tuples just as this for now (!!THIS should be changed to the mkTuple thing)
	| TuplePattern [Pattern]
	| Constructor String [Pattern] deriving (Eq, Show)