module Transpile.Pattern where

import Data.List
import Data.Char

import Text.Show.Pretty

import AST.Pattern
import AST.Module

type Binding = (String, String)
type Predicate = String

--Join multiple predicates into one predicate
writePredicate :: [Predicate] -> Predicate
writePredicate ps = case ps of
	[] -> "true"
	[p] -> p
	_ -> intercalate " && " (map (\p -> "(" ++ p ++ ")")ps)

joinPairs :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
joinPairs (bs1, ps1) (bs2, ps2) = (bs1 ++ bs2, ps1 ++ ps2)

--The "source" string is initially the root variable that the patterns are being extracted from,
--and it is built up upon to handle e.g. cons patterns, tuple patterns etc. 
extractPatternInfo :: Module -> String -> Pattern -> ([Binding], [Predicate])
extractPatternInfo mod source ptrn = case ptrn of
	Wildcard -> ([],[])
	BoolPattern b -> case b of
		False -> ([],['!':source]) 
		True -> ([],[source])
	IntPattern i -> ([],[source ++ " === " ++ show i])
	DoublePattern d -> ([],[source ++ " === " ++ show d])
	CharPattern c -> ([],[source ++ " === \'" ++ [c] ++ "\'"])
	StringPattern str -> ([],[source ++ " === \"" ++ str ++ "\""])
	VarPattern "()" -> ([],[source ++ " instanceof $Unit"])
	VarPattern "[]" -> ([],[source ++ " instanceof $EmptyList"])
	VarPattern "True" -> ([],[source])
	VarPattern "False" -> ([],['!':source])
	VarPattern name@(h:_)
		| isUpper h -> ([],[source ++ " instanceof " ++ name])
		| otherwise -> ([(name, source)],[])
	ConsPattern ptrns -> handleConsPattern mod source 0 ptrns
	SugarList ptrns -> handleSugarListPattern mod source 0 ptrns
	TuplePattern ptrns -> handleTuplePattern mod source 0 ptrns
	Constructor "True" _ -> ([],[source])
	Constructor "False" _ -> ([],['!':source])
	Constructor name ptrns -> case lookupRecordFields name mod of
		Just fields -> handleRecConstructorPattern mod source fields ptrns
		Nothing -> case ptrns of
			[] -> ([],[source ++ " instanceof " ++ name])
			_ -> ([],[source ++ " instanceof " ++ name]) `joinPairs` handleConstructorPattern mod source 0 ptrns

handleConsPattern :: Module -> String -> Int -> [Pattern] -> ([Binding], [Predicate])
--This should never happen?
handleConsPattern _ _ _ [] = ([],[])
handleConsPattern mod source index [p] = case p of
	VarPattern "[]" -> ([], ["length(" ++ source ++ ") === " ++ show (index - 1),"length(" ++ source ++ ") >= " ++ show index])
	VarPattern name -> ([(name, "drop(" ++ show index ++ ")(" ++ source ++ ")")], ["length(" ++ source ++ ") >= " ++ show index])
handleConsPattern mod source index (p:ps) = 
	(extractPatternInfo mod ("$index(" ++ source ++ ")(" ++ show index ++ ")") p) `joinPairs`
	handleConsPattern mod source (index + 1) ps

handleSugarListPattern :: Module -> String -> Int -> [Pattern] -> ([Binding], [Predicate])
handleSugarListPattern mod source index ptrns = case ptrns of
	[] -> ([],["length(" ++ source ++ ") === " ++ show index])
	(p:rest) -> (extractPatternInfo mod ("$index(" ++ source ++ ")(" ++ show index ++ ")") p) `joinPairs` 
		handleSugarListPattern mod source (index + 1) rest

handleTuplePattern :: Module -> String -> Int -> [Pattern] -> ([Binding], [Predicate])
handleTuplePattern mod source index [p] = extractPatternInfo mod (source ++ "[" ++ show index ++ "]") p
handleTuplePattern mod source index (p:ps) = 
	(extractPatternInfo mod (source ++ "[" ++ show index ++ "]") p) `joinPairs`
		handleTuplePattern mod source (index + 1) ps

handleConstructorPattern :: Module -> String -> Int -> [Pattern] -> ([Binding], [Predicate])
handleConstructorPattern mod source index [p] = extractPatternInfo mod (source ++ ".val" ++ show index) p
handleConstructorPattern mod source index (p:ps) = 
	(extractPatternInfo mod (source ++ ".val" ++ show index) p) `joinPairs`
		handleConstructorPattern mod source (index + 1) ps

handleRecConstructorPattern :: Module -> String -> [String] -> [Pattern] -> ([Binding], [Predicate])
handleRecConstructorPattern _ _ [] [] = ([],[])
handleRecConstructorPattern mod source (field:fields) (pat:ptrns) = 
	(extractPatternInfo mod (source ++ "." ++ field) pat) `joinPairs` 
		handleRecConstructorPattern mod source fields ptrns 
handleRecConstructorPattern arg0 arg1 arg2 arg3 = error $ "The non exhaustive args were:\n" ++ intercalate "\n" 
	["ONE: " ++ ppShow arg0, "TWO: " ++ ppShow arg1, "THREE: " ++ ppShow arg2, "FOUR: " ++ ppShow arg3]