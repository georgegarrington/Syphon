module TypeCheck.Common where

import Prelude hiding (id, log)

import qualified Data.Set as S
--import qualified Data.Map as M

import Control.Monad.State.Strict
import Control.Monad.Except

import AST.Type

data Error
	= NotFound String 
	| RecordNotFound String
	| WrongFields [String] --The name of the record with the wrong fields types
	| MissingField
	| NoOptionals String
	| WrongOptField String
	| CaseMismatch --Cases do not all have the same type
	| Misc String
	| Debug String String
	| Mismatch Type Type
	| MismatchInList Type Type
	| SchemeMismatch Scheme Scheme
	| WithLineNum Int Error
	--Used for development, message along with the thingies
	| Dev String [Either (S.Set Constraint) Type]
	deriving (Eq, Ord, Show)

--States that any occurence of the left type (a type variable) should be substituted with the right type
type Substitution = (Type, Type)

type InferenceContext = StateT St (Except Error)

--Data type that the inference function will use as state
data St = St {
	id :: Int, --A counter for unique type variable ids
	log :: [String] --The messages added will be in reverse order
} deriving (Show)

--Generate a fresh type variable and increment the state counter
genFreshVar :: Kind -> InferenceContext Type
genFreshVar kind = do
	freshId <- gets id
	modify $ \st -> st {id = freshId + 1}
	return $ TyVar freshId kind

--Union all given sets in a "left biased" fashion
unionConstraints :: [S.Set Constraint] -> S.Set Constraint
unionConstraints sets = foldl S.union S.empty sets

--Given a type and a list of fresh type variables which are indexed
--in the same order as the quantified variable identifiers, return
--an instantiated type with quantified variables replaced with their fresh type variable instances
instantiate :: Scheme -> InferenceContext Type
instantiate (Scheme kinds ty) = do
	newTyVars <- mapM genFreshVar kinds
	return $ helper ty newTyVars
	where 
		helper (QuantVar i) newVars = newVars !! i
		helper (TyApp lhs rhs) newVars = TyApp (helper lhs newVars) (helper rhs newVars)
		helper ty _ = ty

--Apply all the substitutions found to the type found from inferrence stage
applySubs :: Type -> [Substitution] -> Type
applySubs ty [] = ty
applySubs ty (s:ss) = applySubs (ty `applySubst` s) ss

--Extract the type variables contained within the type in order to make a scheme from it
extractTyVars :: Type -> [Type]
extractTyVars ty = case ty of
	tv@(TyVar {}) -> [tv]
	TyApp t1 t2 -> extractTyVars t1 ++ extractTyVars t2
	_ -> []

applySubst :: Type -> Substitution -> Type
applySubst ty sub@(id, subTy) = case ty of	
	tv@(TyVar {})
		| tv == id -> subTy
		| otherwise -> tv
	--Recursive split; make sure the substitution is applied to all subtypes within a type
	TyApp argTy rtnTy -> TyApp (applySubst argTy sub) (applySubst rtnTy sub)
	ty -> ty