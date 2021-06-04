module TypeCheck.Unification where

import Control.Monad.Except

import qualified Data.Set as S

import AST.Type

import TypeCheck.Common
import TypeCheck.Environment


--Atm the error is just a string, but improve it later to be some data type
type UnificationContext = Except String

doUnification :: Environment -> S.Set Constraint -> Either String [Substitution]
doUnification env constrs = runExcept $ unify env $ S.toList constrs

{-
Clearly there are sort of 2 different types of traits, some traits are just built in like Eq
and Num and stuff, so for now lets just focus on implementing built in traits and then I
think the dictionary passing stuff will sort of come to me if I do this
-} 

unify :: Environment -> [Constraint] -> UnificationContext [Substitution]

unify env [] = return []

--If our final constraint is that 2 type variables are equal when in fact they are different, then clearly
--unification has failed
unify env [Eq tv1@(TyVar {}) tv2@(TyVar {})]
	| tv1 /= tv2 = throwError $ "No unification possible! types are: " ++ show tv1 ++ ", and " ++ show tv2

unify env (Eq (TyApp te1L te2L) (TyApp te1R te2R):constrs) =
	unify env $ Eq te1L te1R:Eq te2L te2R:constrs

unify env cstrs@(Eq tv@(TyVar {}) ty:constrs)
	| ty == tv = unify env constrs
	| ty `containsTyVar` tv = throwError $ "Infinite type error! Constraints are: " ++ show cstrs
	| getKind tv /= getKind ty = throwError "Kind mismatch!"
	--If they are the same then this is no useful information
	--We've found a new mapping in the substitution so apply it to all the constraints we've found
	| otherwise = let subst = (tv, ty) in 
		(:) (tv, ty) <$> unify env (map (\c -> applySubstToConstraint c subst) constrs)
 
unify env cstrs@(Eq ty tv@(TyVar {}):constrs)
	| ty == tv = unify env constrs
	| ty `containsTyVar` tv = throwError "Infinite type error!"
	| getKind ty /= getKind tv = throwError "Kind mismatch!"
	--If they are the same then this is no useful information
	--We've found a new mapping in the substitution so apply it to all the constraints we've found
	| otherwise = let subst = (tv, ty) in 
		(:) (tv, ty) <$> unify env (map (\c -> applySubstToConstraint c subst) constrs)

unify env (Eq c1@(Constr {}) c2@(Constr {}):constrs)
	| c1 == c2 = unify env constrs
	| otherwise = throwError $ "No unification possible, different constructors! Constructors: " ++ show c1 ++ " , and: " ++ show c2

unify env (Eq EVENT t2:constrs) = unify env (Eq (event env) t2:constrs)
unify env (Eq STATE t2:constrs) = unify env (Eq (state env) t2:constrs)
unify env (Eq t1 EVENT:constrs) = unify env (Eq t1 (event env):constrs)
unify env (Eq t1 STATE:constrs) = unify env (Eq t1 (state env):constrs)


{-
unify env (tC@(Eq ty (TraitFun {})):constrs)
	--Solve all other constraints before dealing with trait inference, so put trait constraints to the back of the "queue"
	| not $ onlyTraitConstraintsLeft constrs = unify env (constrs ++ [tC])
	| case ty of {TyApp {} -> False; _ -> True} = throwError "Cannot unify a trait function with a non trait type!"
	|
unify env (tC@(Eq (TraitFun {}) ty):constrs)
	| not $ onlyTraitConstraintsLeft constrs = unify env (constrs ++ [tC])
	| case ty of {TyApp {} -> False; _ -> True} = throwError "Cannot unify a trait function with a non trait type!"
	| 
-}

unify env (Eq t1 t2:constrs) 
	| t1 == t2 = unify env constrs
	| otherwise = throwError $ "No unification possible, different thingies! Thingies: " ++ show t1 ++ " , and: " ++ show t2

--Does the given type contain a type variable with the given identifier?
containsTyVar :: Type -> Type -> Bool
containsTyVar ty target = case ty of
	tv@(TyVar {}) -> target == tv
	(TyApp tyExpr1 tyExpr2) -> 
		(tyExpr1 `containsTyVar` target) || (tyExpr2 `containsTyVar` target)
	--If we see a monotype that is not a type variable then this branch does not contain a type variable
	_ -> False

applySubstToConstraint :: Constraint -> Substitution -> Constraint
applySubstToConstraint (Eq t1 t2) sub = Eq (t1 `applySubst` sub) (t2 `applySubst` sub)