{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Parse.Root where

import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S

import Text.Megaparsec
import Text.Megaparsec.Char


import AST.Definition
import AST.Type
import AST.Module

import Parse.Util
import Parse.Module

--Takes the entire file as a string and attempts to parse it, either returning an error message or the parsed module
parse :: String -> Either String Module
parse source = case result of
	--THe error in string form
	Left err -> Left err 
	Right jumbled -> Right $ unJumble jumbled
	where 
		result = execParser source pJumbled (ParseEnv 0 False)
		-- = runReader (runParserT (greedySc *> pJumbled <* eof) "" source) (ParseEnv 0)

{-
Sort the jumbled up module definitions and put them into their fields
in the module record type. I think this will be useful to have later 
as I reckon some of the simple static checks can be performed whilst going through this,
and also after this is done can check if the required definition fields still have a value
of Nothing and if they do then print an error for the user
-}
unJumble :: Jumbled -> Module
unJumble (name, modulePath, imports, dfns) 
	= helper (Module name modulePath imports [] [] [] [] [] [] M.empty) dfns
	where
		helper mod [] = mod
		helper mod (dfn:dfns) = case dfn of
			Fun name _ _ _ _ -> case name of
				"init" -> helper (mod {specialDfns = dfn:(specialDfns mod)}) dfns
				"update" -> helper (mod {specialDfns = dfn:(specialDfns mod)}) dfns
				"view" -> helper (mod {specialDfns = dfn:(specialDfns mod)}) dfns
				"initEffects" -> helper (mod {specialDfns = dfn:(specialDfns mod)}) dfns
				"effect" -> helper (mod {specialDfns = dfn:(specialDfns mod)}) dfns
				"subscribe" -> helper (mod {specialDfns = dfn:(specialDfns mod)}) dfns
				_ -> helper (mod {funs = dfn:(funs mod)}) dfns
			ADT {} -> helper (mod {adts = dfn:(adts mod)}) dfns
			Record {} -> helper (mod {records = dfn:(records mod)}) dfns
			Alias {} -> helper (mod {aliases = dfn:(aliases mod)}) dfns
			--Even though subscribe definition isnt a function, put it in here anyway as it just fits nicely with the pipeline
			Subscribe {} -> helper (mod {specialDfns = dfn:(specialDfns mod)}) dfns
			Asset {} -> helper (mod {assets = dfn:(assets mod)}) dfns

{-
{-
Sort the jumbled up module definitions and put them into their fields
in the module record type. I think this will be useful to have later 
as I reckon some of the simple modatic checks can be performed whilmod going through this,
and also after this is done can check if the required definition fields modill have a value
of Nothing and if they do then print an error for the user
-}
unJumble :: Jumbled -> Module
unJumble (name, modulePath, imports, dfns) 
	= helper (Module name modulePath imports [] [] [] [] [] M.empty) dfns
	where
		helper mod [] = mod
		helper mod (dfn:dfns) = case dfn of
			Fun name _ _ _ _ -> case name of
				"init" -> helper (mod {specialDfns = dfn}) dfns
				"update" -> helper (mod {specialDfns = dfn}) dfns
				"view" -> helper (mod {specialDfns = dfn}) dfns
				"initEffect" -> helper (mod {specialDfns = dfn}) dfns
				"effect" -> helper (mod {specialDfns = dfn}) dfns
				"subscribe" -> helper (mod {specialDfns = dfn}) dfns
				_ -> helper (mod {funs = dfn:(funs mod)}) dfns
			ADT name _ _ -> helper (mod {adts = dfn:(adts mod)}) dfns
			Record name _ _ -> helper (mod {records = dfn:(records mod)}) dfns
			Alias name _ _ -> helper (mod {aliases = dfn:(aliases mod)}) dfns

-}