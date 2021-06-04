module Main where

import Text.Show.Pretty

import System.Directory
import System.Exit

import CLI.Util
import Parse.Root
import Parse.SUnit
import TypeCheck.Root
import Transpile.SUnit
import Transpile.Root
import Misc.Favourites

main :: IO ()
main = do
	printTitle "Welcome to the Syphon CLI tool"
	source <- readFile "Source.sy"
	let result = parse source
	case result of
		Left err -> do
			putStr err
			exitFailure 
		Right thing -> do
			printTitle "PARSED THING:"
			pPrint thing
	let Right mod = result
	printTitle "RUNNING TYPE SYSTEM"
	let errors = checkModule mod
	printTitle "ERRORS FROM INFERENCE:"
	pPrint errors
	--Check if there is an sunit file in the directory
	dirContents <- getDirectoryContents "."
	let filtered = filter (\str -> str `containsSubList` ".sunit") dirContents
	extra <- case filtered of
		[sunitFile] -> do
			-- let name = snip 5 sunitFile, incase you needed the name :)
			fileStr <- readFile sunitFile
			case parseSUnit fileStr of
				Left err -> putStr err >> exitFailure
				--Implement checking if the name of the module being tested
				--matches the module that is being parsed later
				Right tests -> do
					printTitle "SUNIT FILE DETECTED. MODULE CONTENTS:"
					pPrint $ tests
					return $ writeRunSUnit tests mod
		_ -> return ""
	transpile "syphonDesigns/boilerplate/src/App.js" mod extra
	printTitle "TRANSPILATION SUCCESSFUL."