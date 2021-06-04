--Don't pollute the normal main thing with the tesing stuff, do it all in here
module Testing where

import Control.Monad.Reader

import Text.Megaparsec hiding (parse)
import Text.Show.Pretty

import System.Directory
import System.Exit

--For testing remove later
import qualified Data.Map as M
import qualified Data.Set as S

import AST.Definition
import AST.Expression
import AST.Module as Mod
import AST.Pattern
import AST.SUnit

import TypeCheck.Root
import TypeCheck.Environment
import TypeCheck.Inference
import TypeCheck.Unification

import Transpile.Root
import Transpile.Function
import Transpile.ADT
import Transpile.Primitive
import Transpile.Subscribe
import Transpile.Subscribe2
import Transpile.Pattern
import Transpile.Expression
import Transpile.SUnit

import Parse.Definition
import Parse.Expression
import Parse.Util
import Parse.Root
import Parse.Module
import Parse.Pattern
import Parse.Lit
import Parse.Type
import Parse.SUnit

import PostParse.Root

import Misc.Favourites

--Only test parsing from source.txt file
test :: Show a => Parser a -> IO a
test p = do
	source <- readFile "Source.sy"
	let result = runReader (runParserT (greedySc *> testRemaining p <* eof) "" source) (ParseEnv 0 False)
	case result of
		Left err -> do
			putStr $ errorBundlePretty err
			exitFailure
		Right (thing, unconsumed) -> do
			putStrLn "\nPARSED THING:\n"
			pPrint thing
			putStrLn $ "\nUNCONSUMED TEXT:\n"
			print unconsumed
			putStr "\n"
			return thing

testInfer :: IO ()
testInfer = do
	source <- readFile "Source.sy"
	let result = parse source
	case result of
		Left err -> do
			putStr err
			exitFailure 
		Right thing -> do
			putStrLn "\nPARSED THING:\n"
			pPrint thing
	let Right mod = result
	putStrLn "\nNOW GOING TO TEST INFERENCE...\n"
	let errors = checkModule mod
	putStrLn "\nERRORS FROM INFERENCE:\n"
	pPrint errors

testTranspile :: IO ()
testTranspile = do
	source <- readFile "Source.sy"
	let result = parse source
	case result of
		Left err -> do
			putStr err
			--exitFailure 
		Right thing -> do
			putStrLn "\nPARSED THING:\n"
			pPrint thing
	let Right mod = result
	putStrLn "\nNOW GOING TO TEST TRANSPILING...\n"
	--transpile stuff here
	--HARDCODE THE DIRECTORY LOCATION FOR NOW JUST FOR TESTING, BUT CHANGE THIS LATER
	dirContents <- getDirectoryContents "syphonDesigns/boilerplate/src"
	putStrLn $ "Current directory contents is: " ++ show dirContents
	transpile "syphonDesigns/boilerplate/src/App.js" mod ""
	putStrLn "\nSUCCESSFULLY TRANSPILED :)\n"

properTestTranspile :: IO ()
properTestTranspile = do
	source <- readFile "Source.sy"
	let result = parse source
	case result of
		Left err -> putStr err >> exitFailure
		Right thing -> return ()
			--putStrLn "\nPARSED THING:\n"
			--pPrint thing
	let Right mod = result
	putStrLn "\nNOW GOING TO TEST TRANSPILING...\n"
	dirContents <- getDirectoryContents "."
	print $ "dir contents is: " ++ show dirContents
	let filtered = filter (\str -> str `containsSubList` ".sunit") dirContents
	extra <- case filtered of
		[sunitFile] -> do
			print $ "sunitFile is: " ++ sunitFile
			print $ "filtered is: " ++ show filtered
			-- let name = snip 5 sunitFile, incase you needed the name :)
			fileStr <- readFile sunitFile
			case parseSUnit fileStr of
				Left err -> putStr err >> exitFailure
				--Implement checking if the name of the module being tested
				--matches the module that is being parsed later
				Right tests -> do
					putStrLn "PARSED SUNIT MODULE IS:"
					pPrint $ tests
					return $ writeRunSUnit tests mod
		_ -> return ""
	transpileTest "syphonDesigns/boilerplate/src/App.js" mod extra
	putStrLn "TRANSPILATION SUCCESSFUL"

dummyMod = Module "" [] [] [] [] [] [] [] [] M.empty 

--Pair the parsed data type with the unconsumed text
testRemaining :: Show a => Parser a -> Parser (a, String)
testRemaining p = (,) <$> p <*> many anySingle