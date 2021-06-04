module AST.Module where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import AST.Definition as Dfn
import AST.Expression

{-
All the definitions the user has defined will be bundled together in a module data type
i.e. the type aliases, traits defintiions and trait exhibitions, ADTs, records and
function/variable definitions 
-}

data Module = Module {
	name :: String,
	path :: [String],
	imports :: [Import],
	funs :: [Dfn], 
	adts :: [Dfn], 
	records :: [Dfn], 
	aliases :: [Dfn],
	--Contains init, update, view, effect, subscribe and initEffect
	specialDfns :: [Dfn],
	assets :: [Dfn],
	--For now this is empty as optionals are just hardwires in JS, look into improving this later
	optionals :: M.Map String (M.Map String Expr)
	} deriving (Eq, Show)

data Import = Import {
	modulePath :: [String], --Any modules that the import is contained in seperated by "."
	importName :: String, --The module being imported
	alias :: String, --If you want to explicitly reference the module as an alias, if no alias then ""
	hidden :: [String] --Any definitions hidden from the module
} deriving (Eq, Show)

--Check if the constructor takes arguments or not
checkModADTisFn :: String -> Module -> Maybe Bool
checkModADTisFn name mod = helper name (adts mod)
	where
		helper name [] = Nothing
		helper name (ADT _ _ constrs:adts) = case lookup name constrs of
			Just tys -> case tys of
				[] -> Just False
				_ -> Just True
			Nothing -> helper name adts

--verrrrryy lazily implemented but will do for now :)
checkStateIsRecordType :: Module -> Bool
checkStateIsRecordType mod = (>0) $ length $ getStateFieldNames mod

--Empty list indicates an error
getStateFieldNames :: Module -> [String]
getStateFieldNames mod = helper $ records mod
	where
		helper [] = []
		helper (rc@(Record name _ _):recs)
			| name == "State" = extractFieldNames rc
			| otherwise = helper recs

lookupHelper :: [Dfn] -> String -> Maybe Dfn
lookupHelper [] _ = Nothing
lookupHelper (dfn:dfns) target
	| Dfn.name dfn == target = Just dfn
	| otherwise = lookupHelper dfns target

lookupSpecialDfn :: String -> Module -> Maybe Dfn
lookupSpecialDfn dfnName mod = lookupHelper (specialDfns mod) dfnName

lookupDfn :: String -> Module -> Maybe Dfn
lookupDfn dfnName mod = lookupHelper (funs mod) dfnName

checkIsAsset :: String -> Module -> Bool
checkIsAsset var mod = isJust $ lookupHelper (assets mod) var

lookupRecordFields :: String -> Module -> Maybe [String]
lookupRecordFields name mod = case lookupHelper (records mod) name of
	Nothing -> Nothing
	Just (Record _ _ fields) -> Just $ map fst fields

checkEventIsDefined :: Module -> Bool
checkEventIsDefined mod = 
	(isJust $ lookupHelper (adts mod) "Event") || 
	(isJust $ lookupHelper (records mod) "Event") ||
	(isJust $ lookupHelper (aliases mod) "Event")

checkStateIsDefined :: Module -> Bool
checkStateIsDefined mod =
	(isJust $ lookupHelper (adts mod) "State") || 
	(isJust $ lookupHelper (records mod) "State") ||
	(isJust $ lookupHelper (aliases mod) "State")

--Used mainly for getting event/state definitions out of the module
--There will only ever be one occurrence of each definition in a module so the results of all the searches will (should) always be a singleton list
extractDfn :: String -> Module -> Dfn
extractDfn target mod = head $ map (\selector -> find target (selector mod)) [adts, aliases, records, funs, specialDfns] 
	where
		find :: String -> [Dfn] -> Dfn
		find target [] = error $ "Definition not found: " ++ target ++ ", this should not be possible!"
		find target (dfn:dfns)
			| Dfn.name dfn == target = dfn
			| otherwise = find target dfns