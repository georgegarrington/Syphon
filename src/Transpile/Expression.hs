{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Transpile.Expression where

import Control.Monad (msum)

import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import AST.Expression
import AST.Definition hiding (name)
import AST.Module
import AST.Pattern
import AST.Util

import Transpile.Pattern
import Transpile.Util

import TypeCheck.Environment

{-
Some functions/operators are called something else in the transpiled javascript code
so map any replacements
-}
substitutions :: M.Map String String
substitutions = M.fromList [
	("++", "$append"),
	("!!", "$index"),
	(":", "$_Cons"),
	("+", "$add"),
	("-", "$sub"),
	("*", "$mul"),
	("/", "$div"),
	("%", "$mod"),
	("&&", "$and"),
	("||", "$or"),
	("==", "$eq"),
	("!=", "$nEq"),
	(">", "$gt"),
	("<", "$lt"),
	(">=", "$gtEq"),
	("<=", "$ltEq"),
	("()", "$_Unit()"),
	("[]","$_EmptyList()"),
	("not", "!"),
	("otherwise", "true"),
	("toString", "String"),
	("True","true"),
	("False","false"),
	--Colour ADTS can be replaced with their corresponding Hex strings at compile time:) 
	("Black","\"#000000\""),
	("White","\"#FFFFFF\""),
	("LightGrey","\"#D3D3D3\""),
	("DarkGrey","\"#505050\""),
	("Grey","\"#A9A9A9\""),
	("Yellow","\"#FFFF00\""),
	("Blue","\"#0000FF\""),
	("Red","\"#FF0000\""),
	("Purple","\"#800080\""),
	("Green","\"#000000\""),
	("Pink","\"#FF00FF\""),
	("Orange","\"#F39C12\"")
	]

nativeSubstitute :: String -> String
nativeSubstitute name = 
	case M.lookup name substitutions of
		Just substitution -> substitution
		Nothing -> name

checkADTisFn :: String -> Module -> Bool
checkADTisFn tag mod = 
	case msum [checkModADTisFn tag mod, checkEnvADTisFn tag] of
		Just b -> b
		Nothing -> 
			error $ "ADT could not be found in the module or the base environment!\n This should be impossible!\n\
				\the tag was: " ++ tag

writeExpr :: Int -> Expr -> Module -> String
writeExpr lvl e mod = case e of
	BoolVal b -> case b of
		True -> "true"
		False -> "false"
	IntVal i -> show i
	DoubleVal d -> show d
	CharVal c -> ['\'',c,'\'']
	StringVal s -> "\"" ++ s ++ "\""
	
	Error msg -> "$errMsg(" ++ writeExpr lvl msg mod ++")"

	--If we see a var by itself an it is capitalized then we need to check if it is a function ADT or a non-function ADT
	--(cos if its not a function then needs to be called straight away)
	Var name -> let 
		subbed@(c2:_) = nativeSubstitute name 
		isADT = isUpper (head subbed)
		in
		(if isADT && isAlphaNum c2 then "_" else "") ++ subbed ++ if
			| checkOptional name mod -> "({})"
			| isADT && (not $ checkADTisFn name mod) -> "()"
			| otherwise -> ""
	OptVar name@(c1:_) opt -> let 
		Optional pairs = opt
		subbed@(c2:_) = nativeSubstitute name 
		isADT = isUpper $ head subbed
		in
		(if isADT && isAlphaNum c2 then "_" else "") ++ subbed ++ "({" ++ writeOptional lvl pairs mod ++ "})"
	FieldSelect source field -> writeExpr lvl source mod ++ "." ++ field
	FieldReplace source replacements -> case replacements of
		[(field, newVal)] -> source ++ ".set(\"" ++ field ++ "\", " ++ (writeExpr lvl newVal mod) ++ ")"
		_ -> source ++ ".mergeDeep({" ++ (intercalate ", " $ 
			map (\(fieldName, newVal) -> fieldName ++ " : " ++ writeExpr lvl newVal mod) replacements) ++ "})"
	RecordConstr recName fields -> 
		"new " ++ recName ++ "({" ++ intercalate "," (map (\(name, expr) -> name ++ " : " ++ writeExpr lvl expr mod) fields) ++ "})"
	Tuple exprs -> "[" ++ (intercalate "," (map (\expr -> writeExpr lvl expr mod) exprs)) ++ "]"
	Application fun args -> let
		writeVar = ""
		transpiledArgs = concatMap (\e -> "(" ++ writeExpr lvl e mod ++ ")") args
		in case fun of
			Var name@(c1:_) -> let subbed@(c2:_) = nativeSubstitute name in
				(if isUpper c1 && isAlphaNum c2 then "_" else "") ++ subbed ++ (if checkOptional name mod then "({})" else "") ++ transpiledArgs
			_ -> writeExpr lvl fun mod ++ transpiledArgs

	--Still undecided on which I want to use, I don't think Application serves any benefits tbh
	App e1 e2 -> writeExpr lvl e1 mod ++ "(" ++ writeExpr lvl e2 mod ++ ")"

	{-}
	App e1 e2 -> case e1 of
		Var og@(c1:s) -> 
			(nativeSubstitute $ if isUpper c1 then '_':og else og) ++ 
			(if checkOptional og then "({})" else "") ++
			"(" ++ writeExpr lvl e2 mod ++ ")"
		FieldSelect {} -> writeExpr lvl e1 mod ++ "(" ++ writeExpr lvl e2 mod ++ ")"
		--Any non variable function expression needs to be wrapped in parenths
		_ ->  writeExpr lvl e1 mod ++ "(" ++ writeExpr lvl e2 mod ++ ")" -}
	IfEl pred e1 e2 -> "(() => {if(" ++ writeExpr lvl pred mod  ++ "){\n" ++
		tabs (lvl + 1) ++ "return " ++ writeExpr lvl e1 mod ++ "\n" ++
		tabs lvl ++ "} else {\n" ++
		tabs (lvl + 1) ++ "return " ++ writeExpr lvl e2 mod ++ "\n" ++
		tabs lvl ++ "}})()\n"
	Match matchee cases -> let stringForm = writeExpr lvl matchee mod in
		"(() => {\n" ++ 
		concatMap (\cse -> writeMatchCase (lvl + 1) stringForm cse mod) cases ++
		tabs (lvl + 1) ++ "$error(\"Uncovered match expression case in function:\" + $name + \"!\")\n" ++
		tabs lvl ++ "})()\n"
	Conditional cases -> "(() => {\n" ++
		writeCond (lvl + 2) cases mod ++ 
		--tabs (lvl + 1) ++ "$error(\"Uncovered conditional expression case in function:\" + $name + \"!\")\n" ++
		tabs lvl ++ "})()\n"
	Let ptrn e1 e2 -> let stringForm = writeExpr lvl e1 mod; (bindings, preds) = extractPatternInfo mod stringForm ptrn in
		"(() => {\n" ++
		tabs lvl ++ "if(!(" ++ writePredicate preds ++ ")){$error(\"Uncovered pattern in a let expression in function:\", $name, \"!!\"); return}\n" ++
		writeBindings (lvl + 1) bindings ++ "\n" ++
		tabs (lvl + 1) ++ "return " ++ writeExpr (lvl + 1) e2 mod ++ "\n" ++
		tabs lvl ++ "})()\n" ++ tabs lvl
	Lambda ptrns expr -> let 
		n = length ptrns - 1; 
		bindPredPairs = map (\(source, ptrn) -> extractPatternInfo mod source ptrn) (zip (map (\i -> "arg" ++ show i) [0..n]) ptrns) 
		bindings = concatMap fst bindPredPairs
		preds = concatMap snd bindPredPairs
		in
		"(" ++ concatMap (\i -> "arg" ++ show i ++ " => ") [0..n] ++ "{\n" ++
		tabs (lvl + 1) ++ "if(!(" ++ writePredicate preds ++ ")){$error(\"Uncovered pattern in a lambda expression in function:\", $name, \"!!\"); return}\n" ++ 
		writeBindings (lvl + 1) bindings ++ "\n" ++
		tabs (lvl + 1) ++ "return " ++ writeExpr (lvl + 2) expr mod ++ "\n" ++
		tabs lvl ++ "})\n" ++ tabs lvl --LEAVE THIS HERE!
	--listGen function takes advantage of optional arguments in javascript as no interval may be given
	ListGen e1 interval e2 -> let intervalStr = case interval of Nothing -> ""; Just expr -> ", " ++ writeExpr lvl expr mod in
		"$listGen(" ++ writeExpr lvl e1 mod ++ ", " ++ writeExpr lvl e2 mod ++ intervalStr ++ ")"

	PatternCheck expr ptrn -> let
		transpiled = writeExpr lvl expr mod
		(_,preds) = extractPatternInfo mod transpiled ptrn
		pred = writePredicate preds
		in
		pred

	e -> error $ "uncaught expression! is it: " ++ show e

--Given the list of field name/expression pairs, write an optional object that will override 
--the fields of the default optional object associated with the function
writeOptional :: Int -> [(String, Expr)] -> Module -> String
writeOptional lvl [(name, expr)] mod = name ++ " : " ++ writeExpr lvl expr mod 
writeOptional lvl ((name, expr):rest) mod = 
	name ++ " : " ++ writeExpr lvl expr mod ++ ", " ++ writeOptional lvl rest mod

--Write each of the cases of the cond expression
writeCond :: Int -> [(Expr, Expr)] -> Module -> String
writeCond lvl [] _ = 
	tabs lvl ++ "$error(\"Uncovered case in a conditional expression!\")\n" 
writeCond lvl ((pred, expr):rest) mod = 
	tabs lvl ++ "if(" ++ writeExpr lvl pred mod ++ "){\n" ++
	tabs (lvl + 1) ++ "return " ++ writeExpr lvl expr mod ++ "\n" ++
	tabs lvl ++ "}\n" ++
	writeCond lvl rest mod

writeBindings :: Int -> [Binding] -> String
writeBindings lvl bindings = concatMap (\(alias, val) -> (tabs lvl) ++ "const " ++ alias ++ " = " ++ val ++ "\n") bindings

writeMatchCase :: Int -> String -> (Pattern, Expr) -> Module -> String
writeMatchCase lvl stringForm (ptrn, expr) mod = 
	tabs lvl ++ "if(" ++ writePredicate preds ++ "){\n" ++
	writeBindings (lvl + 1) ++ 
	tabs (lvl + 1) ++ "return " ++ writeExpr lvl expr mod ++ "\n" ++
	tabs lvl ++ "}\n"
	where
		(bindings, preds) = extractPatternInfo mod stringForm ptrn
		writeBindings lvl = 
			concatMap (\(alias, val) -> (tabs lvl) ++ "const " ++ alias ++ " = " ++ val ++ "\n") bindings