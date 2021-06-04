module CLI.Root where

import Control.Monad (when)

import Data.Char
import Data.Either
import Data.Maybe (isJust, fromJust, isNothing)
import Data.IORef

import System.Directory
import System.Environment
import System.Exit
import System.IO.Unsafe

import AST.Module

import CLI.Util

import Parse.Root
import Parse.Util

import TypeCheck.Root
import TypeCheck.Common hiding (Misc)

import Transpile.Root

import Misc.Favourites

{-}

--Return the input and output file path
initCLI :: IO (String, String)
initCLI = do
	putStr "\n"
	printTitle "Welcome to the Syphon CLI tool"
	args <- getArgs
	case args of
		--input path and output path specified in arguments
		[inputPath, outputPath] -> do
			modifyIORef inputRef $ \_ -> inputPath
			modifyIORef outputRef $ \_ -> outputPath
			return ("","")
		--only one path given, assume its the input path
		[inputPath] -> do
			modifyIORef inputRef $ \_ -> inputPath
			--Assume current directory as output path
			modifyIORef outputRef $ \_ -> "."
			return ("","")
		--no args given, check the current directory for a syphon file
		--prompt to use it, if not then ask for input and output directory paths
		[] -> do
			files <- getDirectoryContents "."
			let syphonFiles = filter (\str -> takeFromEnd 3 str == ".sy") files
			case syphonFiles of
				--Assume this is the input file
				[path] -> do
					putStrLn $ "Found " ++ path ++ ", commencing parsing.\n"
					modifyIORef inputRef $ \_ -> path
					modifyIORef outputRef $ \_ -> "."
					return (path, ".")
				--TODO
				(path1:path2:_) -> do
					putStrLn "Multiple Syphon files found in the current directory. Which should I parse?"
					exitSuccess
				--TODO
				_ -> do
					putStrLn "No Syphon files found in the current directory (Did you remember to add the .sy extension?). Please enter a path to a Syphon file:"
					exitSuccess
		--too many args
		_	-> exitSuccess


Takes as input the directory containing the Syphon source code, and also the directory
path where the built Electron application should be placed

runSystem :: String -> String -> IO ()
runSystem inputPath outputPath = do
	let info = (inputPath, outputPath)
	--At the moment only one file is supported
	fileStr <- readFile inputPath
	let parseResult = parse fileStr
	--Can't seem to get fromLeft and fromRight to work for some reason :/
	when (isLeft parseResult) (handleParseErr $ (\(Left err) -> err) parseResult)
	let (Right mod) = parseResult
	when (isNothing $ lookupDfn "init" mod) (errMsg "The mandatory initial state definition \"init\" is not defined!")
	when (isNothing $ lookupDfn "update" mod) (errMsg "The mandatory function definition \"update\" is not defined!")
	when (isNothing $ lookupDfn "view" mod) (errMsg "The mandatory function definition \"view\" is not defined!")
	when (not $ checkEventIsDefined mod) (errMsg "The mandatory Event data type is not defined!")
	when (not $ checkStateIsDefined mod) (errMsg "The mandatory State data type is not defined!")
	let typeErrors = checkModule mod
	when (not $ null typeErrors) (handleTypeErrs typeErrors)
	--All checks passed so now transpile
	transpile outputPath mod

prompt :: IO ()
prompt = do
	putStrLn "Would you like to retry? (y/n)"
	response <- fmap (\(hd:_) -> toLower hd) getLine
	when (not $ response `elem` "yn") $ do
		putStrLn "Invalid response! Please enter \'y\' or \'n\'"
		prompt
	case response of
		'y' -> do
			inputPath <- readIORef inputRef
			outputPath <- readIORef outputRef
			runSystem inputPath outputPath
		'n' -> putStrLn "Goodbye!" >> exitSuccess

errMsg :: String -> IO ()
errMsg s = return () --print "Would you like to retry? (y/n)"

--Change it later to be this
handleParseErr :: {-Parse.Util.ParseErr-} String -> IO ()
handleParseErr err = putStrLn "Parse error!" --print "Would you like to retry? (y/n)"

handleTypeErrs :: [TypeCheck.Common.Error] -> IO ()
handleTypeErrs err = return () --print "Would you like to retry? (y/n)"

--Global variables in Haskell, what a crime :O in this situation it's very convenient though
inputRef :: IORef String
inputRef = newRef ""

outputRef :: IORef String
outputRef = newRef ""

newRef :: a -> IORef a
newRef val = unsafePerformIO $ newIORef val -}