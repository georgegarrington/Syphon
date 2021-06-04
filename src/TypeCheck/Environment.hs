module TypeCheck.Environment where

import Prelude hiding (lookup)

import Control.Monad (msum)

import Data.Maybe
import qualified Data.Map as M

import AST.Definition as Dfn
import AST.Expression
import AST.Type
import qualified AST.Module as Mod

import PostParse.Root

import TypeCheck.Common

{-
Not just a standard environment: also contains information about trait definitions etc.
, but also contains a standard environment of names mapped to schemes
-}
data Environment = Environment {
	{-
	Contains all information about the function names and type signatures in all trait definitions
	-}
	traitSigs :: M.Map String (M.Map String Scheme),
	mapping :: M.Map String Scheme,
	records :: M.Map String (M.Map String Type),
	optionals :: M.Map String (M.Map String Type),
	--Before parsing starts, put the schemes of event and state in here (DONT THINK THIS IS NEEDED?)
	event :: Type,
	state :: Type,
	--FOR TESTING/DEV REMOVE LATER
	msgs :: [String]
} deriving (Show)

--These are the default state and event types, initially they should be nothing or set to e.g. the unit type
baseEnv = Environment initTraitSigs simpleEnv rcrds optionalEnv (Constr "Event" Concrete)  (Constr "State" Concrete) []

simpleEnv 
	= M.fromList $ [
	--	("+", toScheme $ TraitFun "+" $ arrowify $ replicate 3 $ TraitType "Num"),
		--("-", toScheme $ arrowify $ replicate 3 intTy),
		("odd", toScheme $ arrowify [intTy, boolTy]),
		("even", toScheme $ arrowify [intTy, boolTy]),

		("abs", toScheme $ arrowify [intTy, intTy]),
		("+", toScheme $ arrowify $ replicate 3 intTy),
		("-", toScheme $ arrowify $ replicate 3 intTy),
		("*", toScheme $ arrowify $ replicate 3 intTy),
		("/", toScheme $ arrowify $ replicate 3 intTy),

		--Change these to use traits later
		(">", toScheme $ arrowify [intTy, intTy, boolTy]),
		("<", toScheme $ arrowify [intTy, intTy, boolTy]),
		(">=", toScheme $ arrowify [intTy, intTy, boolTy]),
		("<=", toScheme $ arrowify [intTy, intTy, boolTy]),
		("==", toScheme $ arrowify [intTy, intTy, boolTy]),
		("!=", toScheme $ arrowify [intTy, intTy, boolTy]),

		("not", toScheme $ arrowify [boolTy, boolTy]),
		("&&", toScheme $ arrowify $ replicate 3 boolTy),
		("||", toScheme $ arrowify $ replicate 3 boolTy),

		("id", Scheme [Concrete] (arrowify [QuantVar 0, QuantVar 0])),
		("Nothing", Scheme [Concrete] ((Constr "Maybe" $ mkKind 2) `TyApp` (QuantVar 0))),
		("Just", Scheme [Concrete] (arrowify [QuantVar 0, (Constr "Maybe" $ mkKind 2) `TyApp` (QuantVar 0)])),
		("Left", Scheme [Concrete, Concrete] (arrowify [QuantVar 0, (Constr "Either" $ mkKind 3) `TyApp` (QuantVar 0) `TyApp` (QuantVar 1)])),
		("Right", Scheme [Concrete, Concrete] (arrowify [QuantVar 1, (Constr "Either" $ mkKind 3) `TyApp` (QuantVar 0) `TyApp` (QuantVar 1)])),
		(":", Scheme [Concrete] (arrowify [QuantVar 0, listConstr `TyApp` (QuantVar 0), listConstr `TyApp` (QuantVar 0)])),

		("++", Scheme [Concrete, Concrete] (arrowify [listConstr `TyApp` (QuantVar 0), listConstr `TyApp` (QuantVar 0), listConstr `TyApp` (QuantVar 0)])),

		("!!", Scheme [Concrete, Concrete] $ arrowify [listConstr `TyApp` (QuantVar 0), intTy, QuantVar 0]),
		("map", Scheme [Concrete, Concrete] (arrowify [arrowify [QuantVar 0, QuantVar 1], listConstr `TyApp` (QuantVar 0), listConstr `TyApp` (QuantVar 1)])),
		("head", Scheme [Concrete] (arrowify [listConstr `TyApp` (QuantVar 0), QuantVar 0])),
		("drop", Scheme [Concrete] (arrowify [intTy, listConstr `TyApp` (QuantVar 0), listConstr `TyApp` QuantVar 0])),
		("length", Scheme [Concrete] (arrowify [listConstr `TyApp` (QuantVar 0), intTy])),
		("reduce", Scheme [Concrete, Concrete] $ arrowify [arrowify (replicate 3 $ QuantVar 0), listConstr `TyApp` (QuantVar 0), QuantVar 0]),
		("concat", Scheme [Concrete] $ arrowify [listConstr `TyApp` (listConstr `TyApp` (QuantVar 0)), listConstr `TyApp` (QuantVar 0)]),
		("replicate", Scheme [Concrete, Concrete] $ arrowify [intTy, QuantVar 0, listConstr `TyApp` QuantVar 0]),
		("filter", Scheme [Concrete, Concrete] $ arrowify [arrowify [QuantVar 0, boolTy], listConstr `TyApp` QuantVar 0, listConstr `TyApp` QuantVar 0]),
	--	("concat", Scheme [Concrete] $ arrowify [listConstr `TyApp` (listConstr `TyApp` (QuantVar 0)), listConstr `TyApp` (QuantVar 0)]),
--		("replicate", Scheme [Concrete] $ arrowify [intTy, QuantVar 0, listConstr `TyApp` (QuantVar 0)]),
--		("replicateGrid", Scheme [Concrete] $ arrowify [intTy, QuantVar 0, listConstr `TyApp` (listConstr `TyApp` (QuantVar 0))]),
		("append", Scheme [Concrete,Concrete,Concrete] (arrowify [stringTy, stringTy, stringTy])),

		("index2D", Scheme [Concrete,Concrete,Concrete] $ arrowify [listConstr `TyApp` (listConstr `TyApp` (QuantVar 0)), intTy, intTy, QuantVar 0]),
		("replace2D", Scheme [Concrete, Concrete, Concrete, Concrete] $ arrowify 
			[listConstr `TyApp` (listConstr `TyApp` (QuantVar 0)), intTy, intTy, QuantVar 0, listConstr `TyApp` (listConstr `TyApp` (QuantVar 0))]),
		("filter2D", Scheme [Concrete, Concrete] $ arrowify [arrowify [QuantVar 0, boolTy], 
			listConstr `TyApp` (listConstr `TyApp` (QuantVar 0)), listConstr `TyApp` (listConstr `TyApp` (QuantVar 0))]),
		("map2D", Scheme [Concrete, Concrete] $ arrowify [arrowify [QuantVar 0, QuantVar 1], 
			listConstr `TyApp` (listConstr `TyApp` (QuantVar 0)), listConstr `TyApp` (listConstr `TyApp` (QuantVar 1))]),
		("replicate2D", Scheme [Concrete, Concrete] $ arrowify [intTy, QuantVar 0, listConstr `TyApp` (listConstr `TyApp` (QuantVar 0))]),

		("fst", Scheme [Concrete] $ arrowify [mkTuple [QuantVar 0, QuantVar 1], QuantVar 0]),
		("snd", Scheme [Concrete] $ arrowify [mkTuple [QuantVar 0, QuantVar 1], QuantVar 1]),

		("Canvas", Scheme [Concrete, Concrete, Concrete] $ arrowify [intTy, intTy, listConstr `TyApp` (Constr "Shape" Concrete), widgetTy]),
		("Stroke", Scheme [Concrete, Concrete, Concrete] (arrowify [Constr "Color" Concrete, intTy, listConstr `TyApp` (mkTuple [intTy, intTy]), Constr "Shape" Concrete])),
		("Oval", Scheme (replicate 5 Concrete) $ arrowify [Constr "Color" Concrete, intTy, intTy, intTy, intTy, Constr "Shape" Concrete]),
		("Rectangle", Scheme (replicate 5 Concrete) $ arrowify [Constr "Color" Concrete, intTy, intTy, intTy, intTy, Constr "Shape" Concrete]),
		("Point", Scheme (replicate 4 Concrete) $ arrowify [Constr "Color" Concrete, intTy, intTy, intTy, Constr "Shape" Concrete]),

		("Graphic", Scheme [Concrete] $ arrowify [imageTy, widgetTy]),
		("TimerGraphic", toScheme $ imageTy),

		("mkRandomGen", Scheme [Concrete] $ arrowify [intTy, Constr "RandomGen" Concrete]),
		("randomGenBetween", toScheme $ arrowify [intTy, intTy, 
			Constr "RandomGen" Concrete, mkTuple [intTy, Constr "RandomGen" Concrete]]),

		--takes an int to event function (or constructor) and "returns" and effect
		("requestSeed", Scheme [Concrete] $ arrowify [arrowify [intTy, eventTy], effectTy]),


		("[]", Scheme [Concrete] (TyApp listConstr (QuantVar 0))),

		("True", Scheme [Concrete] boolTy),
		("False", Scheme [Concrete] boolTy),
		("otherwise", Scheme [Concrete] boolTy),

		("toString", Scheme [Concrete] (arrowify [intTy, stringTy])),
		("parseInt", Scheme [Concrete] (arrowify [stringTy, intTy])),
		
		--Hardwired stuff that I will keep
		("Column", Scheme [Concrete] (arrowify [TyApp listConstr widgetTy, widgetTy])),
		("Row", Scheme [Concrete] (arrowify [TyApp listConstr widgetTy, widgetTy])),
		("Button", Scheme [Concrete] (arrowify [widgetTy, eventTy, widgetTy])),
		("Text", Scheme [Concrete] (arrowify [stringTy, widgetTy])),
		("TextField", Scheme [Concrete] (arrowify [stringTy, arrowify [stringTy, eventTy], widgetTy])),
		("TextEditor", Scheme [Concrete] (arrowify [stringTy, intTy, intTy, arrowify [stringTy, eventTy], widgetTy])),
		("Container", Scheme [Concrete] (arrowify [widgetTy, widgetTy])),
		("Panel", toScheme $ widgetTy),
		("DummyWidget", toScheme $ widgetTy),

		--Subscriptions
		("UpArrow", Scheme [Concrete] charCodeTy),
		("DownArrow", Scheme [Concrete] charCodeTy),
		("LeftArrow", Scheme [Concrete] charCodeTy),
		("RightArrow", Scheme [Concrete] charCodeTy),
		("KeyChar", Scheme [Concrete] (arrowify [charTy, charCodeTy ])),

		("NoSub", Scheme [Concrete] (Constr "Sub" Concrete)),
		("onTimePassed", Scheme [Concrete] (arrowify [intTy, EVENT, subscriptionTy])),
		("onKeyPressed", Scheme [Concrete] (arrowify [charCodeTy, EVENT, subscriptionTy])),
		("onKeyReleased", Scheme [Concrete] (arrowify [charCodeTy, EVENT, subscriptionTy])),
		("onMouseMoved", Scheme [Concrete] (arrowify [mouseHandler, subscriptionTy])),

		("NoEffect", toScheme $ effectTy),
		("readFile", Scheme [Concrete] $ arrowify [stringTy, arrowify [stringTy, eventTy], effectTy]),
		("writeFile", Scheme [Concrete, Concrete] $ arrowify [stringTy, stringTy, effectTy] )

	] ++ colours

colours = map (\str -> (str, Scheme [Concrete] (Constr "Color" Concrete))) 
	["Black", "Red", "Purple", "Blue", "Green", "Yellow", "Pink", "Orange", "White", "Grey", "DarkGrey", "LightGrey"]

checkEnvADTisFn :: String -> Maybe Bool
checkEnvADTisFn var = case M.lookup var simpleEnv of
	Just (Scheme _ ty) -> Just $ checkIsFnType ty
	_ -> Nothing
	where
		checkIsFnType tyExpr = case tyExpr of
			TyApp ty1 ty2 -> checkIsFnType ty1 || checkIsFnType ty2
			(Constr "(->)" _) -> True
			_ -> False

initTraitSigs
	= M.fromList [
	
	]

optionalEnv = M.fromList 
	[
		("Button", M.fromList [
			("bgColor", Constr "Color" Concrete),
			("dim", mkTuple [intTy, intTy]),
			("rounding", intTy),
			("onRightClick", eventTy)
		]),
		("Text", M.fromList [
			("color", Constr "Color" Concrete)
		]),
		("Canvas", M.fromList[
			("onMousePressed", mouseHandler),
			("onMouseMoved", mouseHandler),
			("onMouseReleased", mouseHandler),
			("bgColor", Constr "Color" Concrete)
		]),
		("Container", M.fromList[
			("dim", mkTuple[intTy,intTy]),
			("bgColor", Constr "Color" Concrete)
		]),
		("Panel", M.fromList[
			("dim", mkTuple [intTy, intTy]),
			("bgColor", Constr "Color" Concrete)
		]),
		("Column", M.fromList[
			("bgColor", Constr "Color" Concrete),
			("gap", intTy)
		]),
		("Graphic", M.fromList[
			("bgColor", Constr "Color" Concrete),
			("dim", mkTuple [intTy, intTy])
		])
	]

checkOptional :: String -> Mod.Module -> Bool
checkOptional varName mod = varName `M.member` optionalEnv || Mod.checkIsAsset varName mod

--Record names mapped to their mapping of their fields paired with their types
rcrds :: M.Map String (M.Map String Type)
rcrds = M.fromList 
 [
	 {-:rt
	 ("State", 
	 M.fromList [
		 ("display", toScheme $ stringTy),
		 --Try replacing this with Operator constructor later and see if unaliasing will handle this, it should do
		 ("operator", toScheme $ arrowify $ replicate 3 intTy),
		 ("operand1", toScheme $ intTy)
	 ]) -}
 ]

--Inject all the required definitions from the module into the base environment before type checking
initEnv :: Mod.Module -> Environment
initEnv mod =
	let 
	--Remove any and all aliases from all type definitions
	aliasMap = M.fromList $ zip (map Dfn.name (Mod.aliases mod)) (map (\aDfn -> ty aDfn) (Mod.aliases mod))
	unaliasedFns = map (unaliaseDfn aliasMap) (Mod.funs mod ++ Mod.specialDfns mod)
	unaliasedAdts = map (unaliaseDfn aliasMap) (Mod.adts mod)
	unaliasedRecords = map (unaliaseDfn aliasMap) (Mod.records mod)
	--No pun intended :)
	unaliasedAliases = map (unaliaseDfn aliasMap) (Mod.aliases mod)
	assetVariables = map name (Mod.assets mod)
	newOpts = M.fromList $ map (\name -> (name, M.singleton "dim" (mkTuple [intTy, intTy]))) assetVariables
	--Bind everything to the environment
	envWithFns = bindMultiple (map (\funDfn -> (name funDfn, sch funDfn)) unaliasedFns) baseEnv
	envWithAdts = bindMultiple (concatMap extractConstrSchemes unaliasedAdts) envWithFns
	envWithRecords = bindRecordDfns unaliasedRecords envWithAdts
	imageSch = toScheme imageTy
	--Bind all the asset variables into the environment with an image type
	envWithAssets = bindMultiple (zip assetVariables (repeat imageSch)) envWithRecords
	envWithAssetOpts = envWithAssets {optionals = M.union newOpts (optionals envWithAssets)}
	--NEED TO IMPLEMENT EXTRACTING EVENT AND STATE TYPES FROM MODULE AND BINDING TO ENVIRONMENT, WILL BE EASY
	in
	envWithAssetOpts {msgs = ["alias map: " ++ show aliasMap, "unaliased fns: " ++ show unaliasedFns, "final env: " ++ show envWithRecords]} --The final environment with all definitions injected into it

--Whether the identifier is a member of the simple mapping part of the environmnet
member :: String -> Environment -> Bool
member id env = id `M.member` (mapping env)

--Looking up the standard part of the environment i.e. the string/scheme mapping
lookup :: String -> Environment -> Maybe Scheme
lookup id env = M.lookup id (mapping env)

--Lookup in the record section of the environment
lookupFromFields :: [String] -> Environment -> Maybe (Scheme, M.Map String Type) 
lookupFromFields fields env = helper fields (M.toList $ records env)
	where
		helper _ [] = Nothing
		helper fields ((recName, fieldMap):pairs) 
			| and $ map (\name -> M.member name fieldMap) fields = 
					--We already know there are fields for the record, so a scheme binding for the 
					--record exists in the normal part of the environment
					Just (toScheme $ Constr recName Concrete,fieldMap)
			| otherwise = helper fields pairs

--Lookup in the optional section of the environment
lookupOptionalFields :: String -> Environment -> Maybe (M.Map String Type)
lookupOptionalFields id env = M.lookup id (optionals env)

--Add a new binding to the scheme mapping part of the environment, at the moment this prevents shadowing
--But I'm not so sure we need to prevent shadowing? Let's see
bind :: String -> Scheme -> Environment -> Environment
bind id sch env = env {mapping = M.insert id sch $ mapping env}

--Insert multiple bindings into the environment at once
--union function is left biased, so put new bindings to the left so that they override
--any duplicate bindings that may exist
bindMultiple :: [(String, Scheme)] -> Environment -> Environment
bindMultiple newOnes env = env {mapping = M.union (M.fromList newOnes) (mapping env)}

bindRecordDfns :: [Dfn] -> Environment -> Environment
bindRecordDfns [] env = env
bindRecordDfns (Record name _ fields:recs) env = 
	env {records = M.insert name (M.fromList fields) (records env), mapping = 
		M.union (mapping env) (M.singleton name (toScheme $ arrowify $ (map snd fields) ++ [Constr name Concrete]))}

--Check if a function is defined as part of a trait, if it is then return the type signature
--of the trait, if it is not then return nothing
checkTrait :: Environment -> String -> Maybe Scheme
checkTrait env id = msum $ M.map (M.lookup id) (traitSigs env)

inject :: Environment -> M.Map String Scheme -> Environment
inject env bindings = env {mapping = M.union (mapping env) bindings}

--If a binding already exists in the evnrionment then it is replaced with the new scheme
overrideBind :: String -> Scheme -> Environment -> Environment
overrideBind id sch env = env {mapping = M.insert id sch $ mapping env}

--insert will replace a value of an entry if it is already present, hence all substitutions will override
--the existing types in the old environment
--We actually want to override any existing bindings in this situation so use overrideBind
substEnv :: Environment -> [Substitution] -> Environment
substEnv env [] = env
substEnv env (((TyVar name kind), newTy):ss) = substEnv (overrideBind (show name) (Scheme [] newTy) env) ss 