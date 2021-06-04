module Transpile.ADT where

import qualified Data.Map as M
import Data.List

import AST.Definition
import AST.Type

import Transpile.Util

writeADT :: Dfn -> String
writeADT (ADT name lineNum constrs) = concatMap writeConstructor constrs
	where
		writeConstructorArgs n = intercalate ", " $ map (\num -> "arg" ++ show num) [0..n]
		writeConstructorBody n = intercalate "; " $ map (\num -> "this.val" ++ show num ++ " = arg" ++ show num) [0..n]
		writeConstructor (tag,tys) = case tys of
			[] -> 
				"class " ++ tag ++ "{}\n\
				\const _" ++ tag ++ " = () => new " ++ tag ++ "()\n"
			_ -> 
				let
				n = length tys - 1
				args = map (\i -> "arg" ++ show i) [0..n]
				in
				"class " ++ tag ++ "{constructor(" ++ writeConstructorArgs n ++ "){" ++ 
				writeConstructorBody (length tys - 1) ++ "}}\n\
				\const _" ++ tag ++ " = " ++ intercalate " => " args ++ " => new " ++ tag ++ "(" ++ 
					writeConstructorArgs n ++ ")\n"

--For the time being, we will use immutable JS records for handling records
writeRecord :: Dfn -> String
writeRecord (Record name lineNum fields) = 
	let
	n = length fields - 1
	fieldNames = map fst fields
	args = map (\i -> "arg" ++ show i) [0..n]
	in
	"class " ++ name ++ " extends Record({" ++ 
		(intercalate ", " $ map (\(fieldName, _) -> fieldName ++ " : undefined" ) fields) ++ "}){}\n" ++
	--Write the function version that can be used as a normal ADT too, args must be given in same order as defined fields
	"const _" ++ name ++ " = " ++ intercalate " => " args ++ " =>\n\
	\\tnew " ++ name ++ "({" ++ (intercalate ", " $ map (\(field, arg) -> field ++ " : " ++ arg) (zip fieldNames args)) ++ "})\n"