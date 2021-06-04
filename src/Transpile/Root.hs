module Transpile.Root where

import Control.Monad (when)

import Data.List
import qualified Data.Map as M
import Data.Maybe

import AST.Definition
import AST.Expression
import AST.Module

import Transpile.ADT
import Transpile.Expression
import Transpile.Subscribe
import Transpile.Primitive
import Transpile.Function


--Hardcode the imports for now
essentials :: String
essentials = 
    "import React from \"react\"\n\
		\import {Record} from \"immutable\"\n\
		\import {$add,$sub,$mul,$div,$mod,$and,$or,not,$eq,$nEq,$gt,$lt,$gtEq,$ltEq,append,$listGen,\n\
		\\tJust, Nothing, _Just, _Nothing, $Unit, $_Unit, fst, snd, $keyExprToCode, _UpArrow, _DownArrow, _LeftArrow, \n\
		\\t_RightArrow, _KeyChar, Left, Right, _Left, _Right, mkRandomGen, randomGenBetween, abs, requestSeed,\n\
		\\treadFile, writeFile, useSyphon, NoEffect, _NoEffect} from \"./lib/Hardwired/Core.js\"\n\
    \import {_Button} from \"./lib/GUI/Button.js\"\n\
    \import {_Text, _TextEditor, _TextField} from \"./lib/GUI/Text.js\"\n\
		\import {_Graphic, _Container, _Panel} from \"./lib/GUI/Misc.js\"\n\
    \import {_Canvas, _Stroke, _Oval, _Rectangle, Stroke, Oval, Rectangle} from \"./lib/GUI/Canvas2.js\"\n\
    \import {_Row, _Column, DummyWidget, _DummyWidget} from \"./lib/GUI/Layout.js\"\n\
    \import {$EmptyList, $_EmptyList, $Cons, $_Cons, $append, concat, $index, map, reduce, length, drop, take, tail, head,\n\
    \\treplace, replace2D, index2D, singleton, $syphon2js, $js2syphon, $syphon2js2D, $js2syphon2D, replicate2D, replicate, filter2D, map2D} from \"./lib/Data/List.js\"\n\
		\import {_SaveIcon, $mkImg} from \"./lib/GUI/Icons.js\"\n\
		\var $errLog = []\n\
		\var $shouldPrint = false\n\
		\const $errMsg = msg => {$errLog.push(msg);if($shouldPrint){console.log(\"\\\"\" + msg + \"\\\"\")}}\n"

--extra bit if there are sunit tests in the directory
transpile :: String -> Module -> String -> IO ()
transpile path mod extra = do
	let context = "export const Context = React.createContext()\n"
	let assetPart = writeAssets $ assets mod
	let adtPart = concatMap writeADT (adts mod)
	let recordPart = concatMap writeRecord (records mod)
	let initDfn = writeFn (fromJust $ lookupSpecialDfn "init" mod) mod
	let updateDfn = writeUpdateFn (fromJust $ lookupSpecialDfn "update" mod) mod
	let (hook, effectPart) = case lookupSpecialDfn "effect" mod of
		Nothing -> ("React.useReducer","")
		Just dfn -> ("useSyphon",writeEffectFn dfn mod)
	putStrLn $ "The hook is: " ++ hook
	--DOESNT SUPPORT CUSTOM HOOK OR SUBSCRIPTION PROPERTIES YET, MAKE SURE TO IMPLEMENT THIS
	let viewDfn = writeReactComponent (fromJust $ lookupSpecialDfn "view" mod) mod hook
	let userDfns = intercalate "\n" $ map (\dfn -> writeFn dfn mod) (funs mod) --SPECIALDFNS BIT JUST FOR TESTING
	let fileStr = intercalate "\n" $ 
		[essentials, assetPart, context, adtPart, recordPart, initDfn,effectPart,updateDfn,viewDfn, userDfns, extra] 
	writeFile path fileStr

transpileTest :: String -> Module -> String -> IO ()
transpileTest path mod extra = let
	assetPart = writeAssets $ assets mod
	adtPart = concatMap writeADT (adts mod)
	recordPart = concatMap writeRecord (records mod)
	normalFns = intercalate "\n" $ map (\dfn -> writeFn dfn mod) (funs mod)
	specialFns = intercalate "\n" $ map (\dfn -> writeFn dfn mod) (specialDfns mod)
	fileStr = intercalate "\n" [essentials,assetPart,adtPart,recordPart,specialFns,normalFns,"\n" ++ extra]
	in
		writeFile path fileStr

writeAssets :: [Dfn] -> String
writeAssets = concatMap $ \(Asset name path) -> 
	"import $asset_" ++ name ++ " from \"" ++ path ++ "\"\n\
	\const " ++ name ++ " = opt => $mkImg(opt)($asset_" ++ name ++ ")\n" 

{-

NO CLUE WHY THIS IS LOOPING :(

--Take the output file path, the module and write the transpiled Javascript code file
transpile :: String -> Module -> IO ()
transpile path mod = do
	
	putStrLn "about to start transpiling"
	let fileStr = essentials ++ "\n"

	let sndLast str = (reverse str) !! 1 

	putStrLn $ 'X':sndLast fileStr:", finished the first thing"

	
	let (subProps, subFns, subHandlers) = case lookupSpecialDfn "subscribe" mod of
		Just {} -> extractSubProps mod mod
		Nothing -> ([],[],[]) 
	
	--Add module level pointer to the dispatch function, initially undefined 
	--but the view function will inject reference upon first render
	let fileStr = fileStr ++ "const $dispatchRef = undefined\n"
	
	putStrLn $ last fileStr:", finished the subscribe init stuff"
	
	--Add all the subscribable properties that were detected
	--let fileStr = fileStr ++ concatMap (\prop -> "const " ++ prop ++ " = undefined\n") subProps
	
	putStrLn $ last fileStr:", just finished the sub properties stuff"

	--Add all the subscription function definitions that are used
	--let fileStr = fileStr ++ concatMap (fnDfns M.!) subFns

	putStrLn $ last fileStr:", just finished the sub functions stuff"

	let fileStr = fileStr ++ concatMap writeADT (adts mod)
	
	putStrLn $ last fileStr:", just finished the adt stuff"

	let fileStr = fileStr ++ concatMap writeRecord (records mod)
	
	putStrLn $ last fileStr:", just finished the records stuff"

	--Optional definitions
	let fileStr = fileStr ++ case lookupSpecialDfn "initEffect" mod of
		Just dfn -> writeFn dfn mod
		Nothing -> ""

	putStrLn $ last fileStr:", just finished the effect thing"

	let fileStr = fileStr ++ case lookupSpecialDfn "subscribe" mod of
		Just dfn -> writeFn dfn mod
		Nothing -> ""
	
	putStrLn $ last fileStr:", just finished the subscribe thing"

	--If the effect function is used then we need to use the custom useSyphon hook instead of useReducer in the view function
	let (hook, fileStr) = case lookupSpecialDfn "effect" mod of
		Just dfn -> ("useSyphon", fileStr ++ writeFn dfn mod)
		Nothing -> ("useReducer", fileStr)

	putStrLn $ last fileStr:", just finished the hook thing"

	--These definitions are mandatory so they must be written
	let fileStr = fileStr ++ writeFn (fromJust $ lookupSpecialDfn "init" mod) mod
	let fileStr = fileStr ++ writeUpdateFn (fromJust $ lookupSpecialDfn "update" mod) mod
	let fileStr = fileStr ++ writeViewFn (fromJust $ lookupSpecialDfn "view" mod) [] mod hook

	putStrLn $ last fileStr:", just finished the mandatory definitions"

	let fileStr = fileStr ++ concatMap (\dfn -> writeFn dfn mod) (funs mod)

	putStrLn $ last fileStr:", just finished the normal functions"

	putStrLn "Finished. File preview:"
	putStr fileStr

	writeFile path fileStr -}
	{-
	let fileStr = fileStr ++ writeAliasDfns module
	let fileStr = fileStr ++ writeADTDfns module
	let fileStr = fileStr ++ writeRecordDfns module
	let fileStr = fileStr ++ writeUpdateFn module
	let fileStr = fileStr ++ writeViewFn module
	let fileStr = fileStr ++ writeUserDfns module
	writeFile path fileStr-}

