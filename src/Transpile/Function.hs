{-# LANGUAGE MultiWayIf #-}
module Transpile.Function where

import AST.Pattern
import AST.Expression
import AST.Definition
import AST.Module

import Transpile.Expression
import Transpile.Pattern
import Transpile.Util

writeFn :: Dfn -> Module -> String
writeFn (Fun name line _ cases wheres) mod =
	let
	Case _ ps body = cases !! 0
	n = length ps
	in
	if
	--The case where it's a variable and not a function
	| n == 0 -> "const " ++ name ++ " = " ++ writeExpr 0 body mod ++ "\n"
	| otherwise -> 
		"const " ++ name ++ " = " ++ writeDfnArgs n ++ "{\n\
		\\tconst $name = \"" ++ name ++ "\"\n" ++ --Have the name in scope of each function for error reporting
		(if length cases == 1 then writeCaseless 1 (cases !! 0) mod else concatMap (\cse -> writeCase 1 cse mod) cases) ++
		"\t$error(\"Uncovered pattern case in function: " ++ name ++ "!\")\n" ++ 
		"}\n"

--If there is only one case of patterns then this is used instead as no if statements are needed
writeCaseless :: Int -> Case -> Module -> String
writeCaseless lvl (Case lineNums ptrns body) mod = 
	let
--	n = length ptrns - 1
	bindPredPairs = map (\(source,ptrn) -> extractPatternInfo mod source ptrn) (zip (map (\i -> "$arg" ++ show i) [0..]) ptrns)
	bindings = concatMap fst bindPredPairs
	preds = concatMap snd bindPredPairs
	(extractedLets, trimmedBody) = extractLeadingLets body
	leadingLetDfns = writeExtractedLets lvl extractedLets mod
	in
	writeBindings lvl bindings ++ 
	leadingLetDfns ++
	tabs lvl ++ "return " ++ writeExpr lvl trimmedBody mod ++ "\n"

writeCase :: Int -> Case -> Module -> String
writeCase lvl (Case lineNum ptrns body) mod = 
	let
--	n = length ptrns -  1
	bindPredPairs = map (\(source, ptrn) -> extractPatternInfo mod source ptrn) (zip (map (\i -> "$arg" ++ show i) [0..]) ptrns)
	bindings = concatMap fst bindPredPairs 
	preds = concatMap snd bindPredPairs
	(extractedLets, trimmedBody) = extractLeadingLets body
	leadingLetDfns = writeExtractedLets lvl extractedLets mod
	in
	tabs lvl ++ "if(" ++ writePredicate preds ++ "){\n" ++
		writeBindings (lvl + 1) bindings ++ 
		leadingLetDfns ++
		tabs (lvl + 1) ++ "return " ++ writeExpr (lvl + 1) trimmedBody mod ++ "\n" ++
	tabs lvl ++ "}\n"


writeExtractedLets :: Int -> [(Pattern, Expr)] -> Module -> String
writeExtractedLets _ [] _ = ""
writeExtractedLets lvl ((ptrn,expr):rest) mod = 
	let
	transpiledExpr = writeExpr lvl expr mod
	(bindings,predicates) = extractPatternInfo mod transpiledExpr ptrn	
	guard
		| null predicates = ""
		| otherwise = "if(!(" ++ writePredicate predicates ++ ")){$error(\"Uncovered pattern in a let expression in function:\", $name, \"!!\"); return}\n"
	in
	tabs lvl ++ guard ++ writeBindings (lvl - 1) bindings ++ writeExtractedLets lvl rest mod

--From an expression with leading lets extract them and remove them,
--returning the list of lets as pattern/expression pairs, paired with
--the expression with the lets removed
extractLeadingLets :: Expr -> ([(Pattern, Expr)], Expr)
extractLeadingLets expr = helper [] expr
	where
		helper acc expr = case expr of
			Let ptrn expr body -> helper (acc ++ [(ptrn,expr)]) body
			_ -> (acc,expr)