module PostParse.Root where

--All checks and manipulation performed on the module after parsing and before inference

import Data.List

import qualified Data.Set as S
import qualified Data.Map as M


import AST.Expression
import AST.Type
import AST.Definition

validSubs = S.fromList [
	"onTimePassed",
	"onMouseMoved",
	"onKeyPressed",
	"onKeyReleased"
	]

--Either empty string or an error string
checkSubFns :: Dfn -> String
checkSubFns (Subscribe _ _ cases _) = 
	let
	--These should ALL be variable expressions with only variable names from validSubs
	firstExprs = (map head . map fst) cases
	msg expr = "Remember that a subscribe block isnt really a proper function!\n\
			\The left hand side must ONLY consist of the following system-primitive\n\
			\subscription generating functions:\n\n" ++ (intercalate ", " $ S.toList $ validSubs) ++ "\n\n\
			\I was expecting one of these functions but instead I saw the expression:\n\n" ++ show expr
	helper [] = ""
	helper (expr@(Var fnName):rest)
		| fnName `S.member` validSubs = helper rest
		| otherwise = msg expr
	helper (wrong:_) = msg wrong
	in
	helper firstExprs

testMap :: M.Map String Type
testMap = M.singleton "State" intTy

unaliaseDfn :: M.Map String Type -> Dfn -> Dfn
unaliaseDfn aliasMap dfn = case dfn of
	Fun {} -> dfn {sch = unaliase aliasMap (sch dfn)}
	ADT {} -> dfn {constructors = 
		map (\(name, tys) -> (name, map (\ty -> unaliase aliasMap ty) tys))(constructors dfn)}
	Record {} -> dfn {fields = map (\(name, ty) -> (name, unaliase aliasMap ty)) (fields dfn)}
	Alias {} -> dfn {sch = unaliase aliasMap (sch dfn)}
	dfn@(Subscribe {}) -> dfn 

class Unaliase a where
	unaliase :: M.Map String Type -> a -> a
instance Unaliase Scheme where
	unaliase aliasMap (Scheme kinds ty) = Scheme kinds (unaliase aliasMap ty)
instance Unaliase Type where
	unaliase aliasMap ty = case ty of
		orig@(Constr potential _) -> case M.lookup potential aliasMap of
			Just alias -> alias
			Nothing -> orig
		TyApp t1 t2 -> TyApp (unaliase aliasMap t1) (unaliase aliasMap t2)
		qv@(QuantVar {}) -> qv
		pq@(ParsedQuant {}) -> pq


--COULD DO WITH SEPERATE FILE MAYBE?

--Given an ADT definition, extract the names and type schemes of the constructor functions
--This needs to be adapted later as currently higher kinded ADTs are not supported
--The name will be the type constructor return type of each of the constructor functions
--At the moment, constructors do not support quantified variables
extractConstrSchemes :: Dfn -> [(String, Scheme)]
extractConstrSchemes (ADT name _ constrs) = map (constrToScheme name) constrs
	where
		constrToScheme ty (name, tys) =
			(name, Scheme (replicate (length tys) Concrete) (arrowify $ tys ++ [Constr ty Concrete]))

{- 

JUNK

unaliasee aliasMap orig@(Constr potential _) = do
	print $ "Found constructor" ++ potential
	case M.lookup potential aliasMap of
		Just alias -> do
			print $ "Lookup successful! Will now replace with: " ++ show alias
			return alias
		Nothing -> do
			print "Nothing in lookup, will return original"
			return orig
unaliasee aliasMap (TyApp t1 t2) = TyApp <$> (unaliasee aliasMap t1) <*> (unaliasee aliasMap t2)
unaliasee aliasMap qv@(QuantVar {}) = return qv
unaliasee aliasMap q@(ParsedQuant {}) = return q -}