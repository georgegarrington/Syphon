module Transpile.Primitive where

import Data.Maybe

import AST.Definition
import AST.Module

import Transpile.Expression
import Transpile.Subscribe2
import Transpile.Function

--Reducer in React useReducer hook can't be a curried function so we have to do this specific syntax 
writeUpdateFn :: Dfn -> Module -> String
writeUpdateFn (Fun name lineNum _ cases wheres) mod = 
	"const update = ($arg0,$arg1) => {\n\n\
	\\tconst $name = \"update\"\n" ++ 
	(if length cases == 1 then writeCaseless 1 (cases !! 0) mod else concatMap (\cse -> writeCase 1 cse mod) cases) ++
	"}\n"

writeEffectFn :: Dfn -> Module -> String
writeEffectFn (Fun name lineNum _ cases wheres) mod =
	"const effect = ($arg0,$arg1) =>{\n\
	\\tconst $name = \"effect\"\n" ++
	(if length cases == 1 then writeCaseless 1 (cases !! 0) mod 
		else concatMap (\cse -> writeCase 1 cse mod) cases) ++
	"}\n"

--Given the view definition and (maybe) a subscribe block definition, write the react component
writeReactComponent :: Dfn -> Module -> String -> String
writeReactComponent fn@(Fun name lineNum _ cases wheres) mod hook = 
	
	"export const App = () => {\n\n\
	\\tconst [state,$dispatch] = " ++ hook ++ "(update,init" ++ (if hook == "useSyphon" then ",effect)" else ")") ++ "\n\
	\\twindow.$dispatchRef = $dispatch\n" ++
	(case lookupSpecialDfn "initEffects" mod of Nothing -> ""; Just dfn -> writeInitEffects dfn mod) ++
	(if isJust $ lookupSpecialDfn "subscribe" mod then writeSubscriptions mod else "") ++ 
	"\treturn (<Context.Provider value = {$dispatch}>{(() => {\n\n\
	\\t\treturn view(state)\n\
	\\n\t})()}</Context.Provider>)\n" ++
	"}\n\n" ++ 
	--The view function
	writeFn fn mod

--There should only ever be one case for initEffect
writeInitEffects :: Dfn -> Module -> String
writeInitEffects (Fun _ lineNum _ [Case _ _ e] wheres) mod = 
	"\tReact.useEffect(() => {\n\
	\\t\tconst effectList = " ++ writeExpr 2 e mod ++ "\n\
	\\t}, [])\n"