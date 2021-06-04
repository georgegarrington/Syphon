module AST.Type where

import Data.List
import Data.Maybe

import Text.Show.Pretty

data Type
	= TyVar Int Kind
	--Once we have parsed all the e.g. as, bs, cs then they can be converted into QuantVars numbered from 0 onwards
	| ParsedQuant Char
	| QuantVar Int
	| Constr String Kind
	| TyApp Type Type
	--As of yet unkown, once parsing is done lookup if it is an alias or anything and replace it
	| Custom String
	| TraitFun String Type String Path
	| TraitType String
	--Special types whose schemes will have been bound to the environment before type checking
	| EVENT
	| STATE
	deriving (Show, Eq, Ord)


--Reconstruct the human readable form of the type from the parse tree
pShowType :: Type -> String
pShowType ty = case ty of
	TyApp (Constr "(->)" _) ty -> pShowType ty
	TyApp (Constr "[]" _) ty -> "[" ++ pShowType ty ++ "]"
	TyApp (Constr id _) ty -> id ++ " " ++ pShowTyArgs ty 
	TyApp t1 t2 -> pShowType t1 ++ " -> " ++ pShowType t2
	tv@(TyVar i kind) -> "(TyVar " ++ show i ++ " " ++ show kind ++ ")"
	Constr id _ -> id
	ParsedQuant ch -> [ch]
	QuantVar i -> show i

pShowTyArgs :: Type -> String
pShowTyArgs ty = case ty of
	TyApp t1 t2 -> pShowType t1 ++ " " ++ pShowTyArgs t2
	ty -> pShowType ty

data Scheme = Scheme [Kind] Type deriving (Eq, Ord)

instance Show Scheme where
	show (Scheme kinds ty) = "Scheme " ++ show kinds ++ " :: " ++ show ty

toScheme :: Type -> Scheme
toScheme ty = Scheme [] ty

data Kind
	= Concrete
	| Abstract Kind Kind deriving (Eq, Ord, Show)

--This is all a constraint is for the time being
data Constraint 
	= Eq Type Type deriving (Show, Eq, Ord)

type Path = [Dir]

{-
The inference algorithm keeps track of a path to get to a variable name, so that when it looks
the variable up in the environment if it turns out to be a trait function, it can return this path
once inference is finished paired with the specific trait type definition that was found so that the expression
tree can be modified by following the path and e.g. eq can be changed into eqInt, etc. 
-}
--THIS SHOULD PROBABLY BE CHANGED TO BE JUST NUMBERS LATER
data Dir
	= Trivial --The case where there is only one possible path e.g. a lambda
	| If1st
	| If2nd
	| If3rd
	--Used for conditional and match expressions, which index was taken
	| CondPredIndex Int
	| CondCaseIndex Int
	| MatchIndex Int
	| MatcheeExpr
	| TupleLeft
	| TupleRight
	| Let1st
	| Let2nd
	| App1st
	| App2nd 
	| RecLeft
	| Ind Int
	| RecField String deriving (Eq, Ord, Show)

getKind :: Type -> Kind
getKind ty = case ty of
	TyVar _ kind -> kind
	Constr _ kind -> kind
	TyApp lhs rhs -> case getKind lhs of
		(Abstract star kind) -> kind
		_ -> Concrete
	_ -> Concrete

--Tuple constructor applied to arbitrarily many types
mkTuple :: [Type] -> Type
mkTuple tys = helper $ reverse $ (Constr ("(" ++ replicate (n-1) ',' ++ ")") (mkKind n)):tys
	where
		n = length tys + 1
		{-
		helper [ty] = ty
		helper (ty:tys) = ty `TyApp` helper tys-} 


		helper [fstTy, ty] = TyApp ty fstTy
		helper (back:reversed) = TyApp (helper reversed) back

--These should probably be moved later
unitTy = Constr "()" Concrete
charTy = Constr "Char" Concrete
intTy = Constr "Int" Concrete
doubleTy = Constr "Double" Concrete
boolTy = Constr "Bool" Concrete
stringTy = Constr "String" Concrete
widgetTy = Constr "Widget" Concrete
eventTy = Constr "Event" Concrete

subscriptionTy = Constr "Subscription" Concrete

charCodeTy = Constr "CharCode" Concrete

effectTy = Constr "Effect" Concrete

imageTy = Constr "Image" Concrete

--Takes a concrete getKind and produces a concrete getKind
listConstr = Constr "[]" $ mkKind 2

--Makes sense, an arrow type constructor e.g. a -> b takes two concrete kinded types
--a and b in order to produce a new concrete kinded type, a -> b
arrowConstr = Constr "(->)" $ mkKind 3

--Takes 1 a, 1 b and returns an (a,b) type
--tupleConstr = Constr "(,,)" $ mkKind 3
maybeConstr = Constr "Maybe" $ mkKind 2
eitherConstr = Constr "Either" $ mkKind 3

a `tyFun` b = TyApp (TyApp arrowConstr a) b

emptyList = Scheme [Concrete] 

canvasTy = arrowify [intTy, intTy, mouseHandler, mouseHandler, mouseHandler, listConstr `TyApp` (Constr "Stroke" Concrete), widgetTy]
mouseHandler = arrowify [intTy, intTy, EVENT]


--list as a type function: [] :: ArrTy (TyVar "a") ()

mkKind :: Int -> Kind
mkKind 1 = Concrete
mkKind n = Abstract Concrete $ mkKind $ n - 1

{-
Helper function that turns e.g. [IntTy, IntTy, IntTy] (Int -> Int -> Int) into
IntTy `ArrTy` (IntTy `ArrTy` IntTy)
-}
arrowify :: [Type] -> Type
arrowify [t] = t
arrowify (t:ts) = t `tyFun` (arrowify ts)

--The first type applied to several types, DONT THINK THIS IS NEEDED?
construct :: [Type] -> Type
construct [t1, t2] = TyApp t1 t2
construct (t:ts) = TyApp t $ construct ts

--Get the list of all the parsed quants that appear in the type and turn them into digits
digitify :: Type -> Type
digitify ty = replace tyVarMap ty
	where
		extractor ty = case ty of
			ParsedQuant c -> [c]
			TyApp t1 t2 -> extractor t1 ++ extractor t2
			_ -> []
		tyVarMap = zip (nub $ extractor ty) [0..]
		replace map ty = case ty of
			ParsedQuant c -> QuantVar $ fromJust $ lookup c map
			TyApp t1 t2 -> TyApp (replace map t1) (replace map t2)
			ty -> ty