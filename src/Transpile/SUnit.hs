module Transpile.SUnit where

import Data.Maybe
import Data.List

import AST.SUnit
import AST.Module

import Transpile.Expression
import Transpile.Pattern

import Misc.Favourites

--Write the run sUnit function which will go at the bottom of the file
writeRunSUnit :: [TestDfn] -> Module -> String
writeRunSUnit tests mod = let
	myVal = 1
	in
	"function $runSUnit(){\n\
	\\tlet $successes = 0\n\
	\\tlet $failures = 0\n\
	\\tconst $isFailure = res => res === undefined || Number.isNaN(res) || res === null\n\
	\\tconst $sep = () => console.log(\"--------------------------------------------------\")\n\
	\\tfunction $checkSatisfies(result, pred){\n\
	\\t\tif(pred(result)){\n\
	\\t\t\tconsole.log(\"\\tSUCCESS: result satisfied predicate.\")\n\
	\\t\t\t$successes++\n\
	\\t\t} else {\n\
	\\t\t\tconsole.log(\"\\tFAILURE: result did not satisfy predicate.\")\n\
	\\t\t\t$failures++\n\
	\\t\t}\n\
	\\t}\n\
	\\tfunction $verify(result, ptrnPred, err = undefined){\n\
	\\t\tif(err != undefined){\n\
	\\t\t\tconst actualErr = $errLog.pop()\n\
	\\t\t\tif(actualErr === err){\n\
	\\t\t\t\tconsole.log(\"\\tSUCCESS: failed with expected message.\")\n\
	\\t\t\t\t$successes++\n\
	\\t\t\t} else {\n\
	\\t\t\t\tconsole.log(\"\\tFAILURE: function did not fail with expected message.\")\n\
	\\t\t\t\tconsole.log(\"\\tActual error message: \\\"\" + actualErr + \"\\\"\")\n\
	\\t\t\t\t$failures++\n\
	\\t\t\t}\n\
	\\t\t} else {\n\
	\\t\t\tif(ptrnPred(result)){\n\
	\\t\t\t\tconsole.log(\"\\tSUCCESS: function returned expected value.\")\n\
	\\t\t\t\t$successes++\n\
	\\t\t\t} else {\n\
	\\t\t\t\tconsole.log(\"\\tFAILURE: function did not return expected value.\")\n\
	\\t\t\t\tconsole.log(\"\\tActual return value:\")\n\
	\\t\t\t\tconsole.log(result)\n\
	\\t\t\t\t$failures++\n\
	\\t\t\t}\n\
	\\t\t}\n\
	\\t}\n" ++ 
	concatMap (\tst -> writeUnitTest tst mod) tests ++
	"\t$sep()\n\
	\\tconsole.log(\"TESTING COMPLETE\")\n\
	\\t$sep()\n\
	\\tconsole.log(($successes + $failures) + \" total tests ran\")\n\
	\\tconsole.log($successes + \" successes\")\n\
	\\tconsole.log($failures + \" failures\\n\")\n\
	\}"

writeUnitTest :: TestDfn -> Module -> String
writeUnitTest (GlobalLet ptrn body) mod = writeBindings 1 bindings
	where
		transpiled = writeExpr 0 body mod
		(bindings,_) = extractPatternInfo mod transpiled ptrn
writeUnitTest (Test name cases) mod = 
	"\t$sep()\n\
	\\tconsole.log(\"RUNNING " ++ show (length cases) ++ " TESTS FOR FUNCTION: " ++ name ++ "\")\n\
	\\t$sep()\n" ++ 
	concatMap (\(cse,num) -> writeTestCase cse num) (zip cases [1..])
	where
		writeArgs args
			--React reducers must use proper function syntax instead of (..)(..)
			--DONT THINK WE NEED THIS
	--		| name == "update" = "(" ++ writeExpr 2 arg0 mod ++ "," ++ writeExpr 2 arg1 mod ++ ")"
			| otherwise = concatMap (\arg -> "(" ++ writeExpr 2 arg mod ++ ")") args
		writeTestCase (LocalLet ptrn body) _ = (writeBindings 1 bindings) -- ++ (error $ "Bindings are: " ++ show bindings)
			where
				transpiled = writeExpr 0 body mod
				(bindings, _) = extractPatternInfo mod transpiled ptrn
		writeTestCase (TestCase stringArgs stringPtrn args expected) num = 
			"\tconsole.log(\"" ++ show num ++ ":\")\n\
			\\tconsole.log(\"\\tInputs = " ++ stringArgs ++ ", " ++ writeExpected ++ "\")\n" ++
			if isPredicate expected then "\t$checkSatisfies(" ++ name ++ writeArgs args ++ "," ++ writeExpr 1 (getPredExpr expected) mod ++")\n" else 
			"\t$verify(" ++ name ++ writeArgs args ++ "," ++ writeVerifier expected  ++ ")\n"
			where
				writeExpected = case expected of
					--Expecting error so surroung in quotes
					Failure {} -> "Expected error message: \\\"" ++ (stringPtrn `dropAnyOf` '\"') ++ "\\\""
					--No string so dont surround
					ReturnPtrn {} -> "Expected return value: " ++ stringPtrn
					Predicate {} -> "Expecting satisfaction of: " ++ stringPtrn
				isPredicate thing = case thing of
					Predicate {} -> True
					_ -> False
				getPredExpr (Predicate expr) = expr
				writeVerifier expected = case expected of
					Failure str -> "undefined, \"" ++ str ++ "\""
					ReturnPtrn ptrn -> "res => " ++ (writePredicate $ snd $ extractPatternInfo mod "res" ptrn)