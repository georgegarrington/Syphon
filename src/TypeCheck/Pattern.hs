-- :( dunno why this isnt working
module TypeCheck.Pattern where

import Prelude hiding (lookup, id)

import Control.Monad.Except

import qualified Data.Set as S
import qualified Data.Map as M

import AST.Type
import AST.Pattern

import TypeCheck.Common
import TypeCheck.Environment

type Bindings = M.Map String Scheme

inferPattern :: Environment -> Pattern -> InferenceContext (Type, S.Set Constraint, Bindings)
inferPattern env pat = case pat of
	Wildcard -> do
		newTyVar <- genFreshVar Concrete
		return (newTyVar, S.empty, M.empty)
	--The only pattern that generates an environment binding
	VarPattern id -> do
		newTyVar <- genFreshVar Concrete
		return (newTyVar, S.empty, M.singleton id $ toScheme newTyVar)
	IntPattern _ -> return (intTy, S.empty, M.empty)
	DoublePattern _ -> return (doubleTy, S.empty, M.empty)
	CharPattern _ -> return (charTy, S.empty, M.empty)
	StringPattern _ -> return (stringTy, S.empty, M.empty)
	ConsPattern ptrns -> do
		newTyVar <- genFreshVar Concrete
		results <- mapM (inferPattern env) ptrns
		let tys = map frst results
		let resConstrs = map scnd results
		let bindings = unionBindings $ map thrd results
		let constraints 
			= unionConstraints $
				(S.fromList $ Eq (TyApp listConstr newTyVar) (last tys):map (Eq newTyVar) (init tys)): 
				resConstrs
		return (TyApp listConstr newTyVar, constraints, bindings)
	--The type rule for sugar list is the same as cons with empty list on the end so just do that
	SugarList ptrns -> inferPattern env (ConsPattern $ ptrns ++ [VarPattern "[]"])
	TuplePattern ptrns -> do
		info <- mapM (inferPattern env) ptrns
		let tys = map frst info
		let constraints = unionConstraints $ map scnd info
		let bindings = unionBindings $ map thrd info
		return (mkTuple tys, constraints, bindings)
	Constructor id ptrns -> case lookup id env of
		Just sch -> do
			--Find the type of the constructor function
			instnce <- instantiate sch
			results <- mapM (inferPattern env) ptrns

			--Our assumption for the type of the constructor function applied to all it's arguments
			newTyVar <- genFreshVar Concrete
			--throwError $ Misc $ "Old environment: " ++ (show $ mapping env) ++ " \n\n New environment:" ++ (show $ mapping newEnv)
			--The types of each of the pattern
			let tys = map frst results
			--Now assume that the instance from environment is equal to 
			--some type variable applied to the types of each individual pattern
			let constraints = unionConstraints $ (S.singleton $ Eq (arrowify $ tys ++ [newTyVar]) instnce):(map scnd results)
			let bindings = unionBindings $ map thrd results
			--when(id /= "Just") (throwError $ Misc $ "The constraints for the constructor are: " ++ show constraint)
			return (newTyVar, constraints, bindings)
		Nothing -> throwError $ NotFound id

frst :: (a,b,c) -> a
frst (x,_,_) = x

scnd :: (a,b,c) -> b
scnd (_,y,_) = y

thrd :: (a,b,c) -> c
thrd (_,_,z) = z

unionBindings :: Ord a => [M.Map a b] -> M.Map a b
unionBindings maps = foldl M.union M.empty maps