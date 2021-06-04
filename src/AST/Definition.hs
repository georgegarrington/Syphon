module AST.Definition where

import qualified Data.Set as S

import AST.Type
import AST.Expression
import AST.Pattern

--Make the subscribe definition have a name of "subscribe" just so it fits in nicely with the rest of the pipeline
data Dfn
	= Fun {name :: String, line :: Int, sch :: Scheme, cases :: [Case], wheres :: [Dfn]}
	| ADT {name :: String, line :: Int, constructors :: [Constructor]}
	| Record {name :: String, line :: Int, fields :: [(String, Type)]}
	| Alias {name :: String, line :: Int, ty :: Type} 
	| Subscribe {name :: String, line :: Int, subCases :: [([Expr], Expr)], whrs :: [([Pattern], Expr)]} 
	| Asset {name :: String, path :: String} deriving (Show, Eq)

--Line number, argument patterns and body expression
data Case = Case Int [Pattern] Expr deriving (Show, Eq)

type Constructor = (String, [Type]) 

--Given a record, extract all the field names
extractFieldNames :: Dfn -> [String]
extractFieldNames (Record _ _ fields) = map fst fields