module TypeCheck.Inference where

import Prelude hiding (log, id, lookup)

import Control.Monad.Except
import Control.Monad.State.Strict

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Either (isLeft)

import AST.Definition
import AST.Expression
import AST.Type
import AST.Pattern
import AST.Util

import TypeCheck.Common
import TypeCheck.Environment
import TypeCheck.Pattern
import TypeCheck.Unification

import Misc.Favourites

doInferCase :: Environment -> String -> Case -> Either Error (Type, S.Set Constraint)
doInferCase env funName (Case lineNum ptrns body) = 
	let
	result = (runExcept $ runStateT (inferCase env funName ptrns body) (St 0 [])) 
	in
	case result of
		Left err -> Left err
		--Change the monad transformer so it does not return the state because we don't need it
		Right stateful -> Right $ fst stateful
			
inferCase :: Environment -> String -> [Pattern] -> Expr -> InferenceContext (Type, S.Set Constraint)
inferCase env funName ptrns body = do
	--Our assumption of the return type of the function
	results <- mapM (inferPattern env) ptrns
	--We know that the function name will be in the environment as all module defined functions
	--must have type signatures, and these are injected into the environment before type checking
	expectedTy <- instantiate $ fromJust $ lookup funName env
	let ptrnTys = map frst results
	let ptrnConstrs = map scnd results
	let ptrnBindings = unionBindings $ map thrd results
	(bodyTy, bodyConstrs) <- infer [] (env `inject` ptrnBindings) body
	let ty = arrowify $ ptrnTys ++ [bodyTy]
	let constraints = unionConstraints $ S.singleton (Eq ty expectedTy):bodyConstrs:ptrnConstrs

	return (ty, constraints)

--Think this might be useless? NOPE :)
doInferExpr :: Environment -> Expr -> Either Error (Type, S.Set Constraint)--((Type, S.Set Constraint), St)
doInferExpr env expr = let res = runExcept $ runStateT (infer [] env expr) (St 0 []) in
	case res of
		Left err -> Left err
		Right stateful -> Right $ fst stateful

doTypeTest :: Expr -> Either Error Type
doTypeTest e = case doInferExpr baseEnv e of
	Left err -> Left err
	Right (ty,constrs) -> case doUnification baseEnv constrs of
		Left errMsg -> Left $ Misc errMsg
		Right sub -> Right $ applySubs ty sub

--Either return an error or the inferred type paired with the constraints that
--must hold in order for the expression to be well types and for which a unification 
--must exist that can be solved on the system of constraints which will determine the function's type
--Work in a special context that handles storing our underlying state of the type variable id that can be incremented
--whenever we need a fresh type variable, and also allows for exceptions to be thrown
infer :: Path -> Environment -> Expr -> InferenceContext (Type, S.Set Constraint)
infer path env expr = case expr of

	--Int and boolean valures are inferMonadred as being int and bool types,
	--And there are no constraints required to hold for this to be true
	IntVal _ -> return (intTy, S.empty)
	DoubleVal _ -> return (doubleTy, S.empty)
	BoolVal _ -> return (boolTy, S.empty)
	CharVal _ -> return (charTy, S.empty)
	StringVal _ -> return (stringTy, S.empty)

	--Does not make sense for an error expression to evaluate to a type so just generate a fresh variable
	Error msgExpr -> do
		(msgTy, _) <- infer path env msgExpr
		newTyVar <- genFreshVar Concrete
		--The message expression must evaluate to a string type, but thats it
		return (newTyVar, S.singleton $ Eq msgTy stringTy)

	Var id -> case checkTrait env id of
		Just sch -> do
			instnce <- instantiate sch
			--The only place that a path is used, use it to retrace where the trait function
			--is used in the expression and replace it with the specific trait definition of the function after type inference
			return (TraitFun id instnce "" path, S.empty)
		Nothing -> case lookup id env of
			Just sch -> do
				--throwError $ Misc $ "The scheme is: " ++ show sch
				instnce <- instantiate sch
				--throwError $ Misc $ "The instance is: " ++ show instnce
				return (instnce, S.empty)
			Nothing -> throwError $ NotFound id

	--Same as a variable but contains an optional structure
	OptVar id optional -> case checkTrait env id of
		Just sch -> do
			instnce <- instantiate sch
			return (TraitFun id instnce "" path, S.empty)
		--Lets forget about traits for now
		Nothing -> case lookup id env of
			--The variable's type that is stored in the environment, instantiated
			Just sch -> instantiate sch >>= \instnce -> 
				case lookupOptionalFields id env of

				--This variable was not found to contain an optional structure
				Nothing -> throwError $ NoOptionals id
				Just tys -> do
					--The names given for each field, they should match the field names stored in the environment
					let optFields = (\(Optional fields) -> fields) optional
					let names = map fst optFields
					
					--The inferred types for each expression in the optional structure, in the same order as the names
					observedTysConstraints <- mapM (\(_, expr) -> infer (Trivial:path) env expr) optFields
					let observedTys = map fst observedTysConstraints
					let constrs = map snd observedTysConstraints

					--The lookup results of the actual types of each of the fields wrapped in a just, if 
					--any of the fields were not found then a nothing is returned
					let actualTys =  map (\name -> M.lookup name tys) names 
					case Nothing `elem` actualTys of
						True -> throwError $ WrongOptField id
						False -> do
							let constraints = unionConstraints $ 
								(S.fromList $ zipWith Eq observedTys (map fromJust actualTys)):constrs
							return (instnce, constraints)
				{-do
				instnce <- instantiate sch
				return (instnce, S.empty) -}
			Nothing -> throwError $ NotFound id

	IfEl pred e1 e2 -> do

		--We have no idea what the type of the if expression will be
		--so generate a fresh variable to represent the type of the if expression
		newTyVar <- genFreshVar Concrete
		(predTy, predConstraints) <- infer (If1st:path) env pred
		(e1Ty, e1Constraints) <- infer (If2nd:path) env e1
		(e2Ty, e2Constraints) <- infer (If3rd:path) env e2

		let ifExprConstraints 
			= unionConstraints [
				--Makes sense, the predicate expression of an if expression must be a bool
				--and both branches of the if expression must have the same type, which will 
				--ultimately be the type the whole if expression evaluates to. Also take into account
				--constraints that inferring each of the 3 expressions may have generated 
				S.fromList [Eq predTy boolTy, Eq newTyVar e1Ty, Eq newTyVar e2Ty],
				predConstraints,
				e1Constraints, 
				e2Constraints
			]

		return (newTyVar, ifExprConstraints)

	Conditional pairs -> do

		newTyVar <- genFreshVar Concrete
		predExprs <- mapM (\(predExpr, i) -> infer (CondPredIndex i:path) env predExpr) (zip (map fst pairs) [0..])
		let predTypes = map fst predExprs
		let predConstraints = concat $ map (S.toList . snd) predExprs
		sndExprs <- mapM (\(caseExpr, i) -> infer (CondCaseIndex i:path) env caseExpr) (zip (map snd pairs) [0..])
		let sndTypes = map fst sndExprs
		let sndConstraints = concat $ map (S.toList . snd) sndExprs 
		let condExprConstraints 
			= unionConstraints [
				{-
				Makes sense, in all the pairs of a conditional expression, all the of the "fst" of each expression
				should be a boolean as they are all predicates, and every sub "return" expression within the conditional
				expression must all have the same type, which is the new type variable we have generated 
				-}
				S.fromList $ map (Eq boolTy) predTypes,
				S.fromList $ map (Eq newTyVar) sndTypes,
				S.fromList predConstraints,
				S.fromList sndConstraints
			]
		return (newTyVar, condExprConstraints)

	Match matchee pairs -> do
		
		let handleMatchCase matcheeType path index env (ptrn, expr) = do
			(ty, ptrnConstraints, ptrnBindings) <- inferPattern env ptrn
			--throwError $ Misc $ "Finished the pattern in the match case and the environment is: " ++ show (mapping newEnv)
			--Record the path incase we come across a trait function in the match case body expression
			(caseTy, caseConstraints) <- infer (MatchIndex index:path) (env `inject` ptrnBindings) expr
			--The type of a match expression is the type of each of the "return" expressions, which must all be the same and 
			--also be the same type as the matchee expression being matched
			return  (caseTy, unionConstraints [ptrnConstraints, caseConstraints, S.singleton $ Eq matcheeType ty])

		--The type that is inferred for the matchee expression is the type of the whole match expression
		--and also the type of each of the inner case expressions
		(matcheeTy, matcheeConstrs) <- infer (MatcheeExpr:path) env matchee 
		--The type we will assume is the type of the whole match expression, which is subsequently the type
		--of each of the "return" expressions in the match expression, which obviously all must be the same type
		newTyVar <- genFreshVar Concrete
		caseInfo <- mapM (\(index, tup) -> handleMatchCase matcheeTy path index env tup) $ zip [0..] pairs
		let constraints = unionConstraints $
			--The type of each of the expression in the match case MUST be the
			--same, and this will ultimately be the type of the entire match expression
			matcheeConstrs:[S.fromList $ (map (Eq newTyVar) (map fst caseInfo))] ++ (map snd caseInfo)
		return (newTyVar, constraints)		

	Lambda ptrns body -> do

		results <- mapM (inferPattern env) ptrns
		let ptrnBindings = unionBindings $ map thrd results
		--Basically, whenever you are unsure what the type of something is,
		--generate a new type variable to represent the unknown type
		(bodyTy, bodyConstrs) <- infer (Trivial:path) (env `inject` ptrnBindings) body
		--A function type taking input types of each of the arguments and returning the type of the body expression
		let ty = arrowify $ (map frst results) ++ [bodyTy]
		let constraints = unionConstraints $ map scnd results ++ [bodyConstrs]
		return (ty, constraints) 

	--Let the type system handle turning it into the application format below
	Application e es -> infer path env $ applicationify $ e:es

	App e1 e2 -> do

		newTyVar <- genFreshVar Concrete
		(e1Ty, e1Constraints) <- infer (App1st:path) env e1
		(e2Ty, e2Constraints) <- infer (App2nd:path) env e2

		let appConstraints
			= unionConstraints [
				S.singleton $ Eq e1Ty (e2Ty `tyFun` newTyVar),
				e1Constraints,
				e2Constraints
			]

		return (newTyVar, appConstraints)
	
	
	Tuple exprs -> do
		let n = length exprs
		--assums <- replicateM n $ genFreshVar Concrete
		--Insert index info into the path later, this is fine for now
		tyConstrPairs <- mapM (infer (Trivial:path) env) exprs
		let tys = map fst tyConstrPairs
		--let assumConstrs = S.fromList $ zipWith Eq assums tys
		let constrs = map snd tyConstrPairs
		return (mkTuple tys, unionConstraints constrs) --try cons assumConstrs: if doesnt work, dont think we need to generate the type variables though

{-
	PairTuple e1 e2 -> do
		--The types we will assume each side of the tuple to be
		e1Assum <- genFreshVar Concrete
		e2Assum <- genFreshVar Concrete
		(e1Ty, e1Constrs) <- infer (TupleLeft:path) env e1
		(e2Ty, e2Constrs) <- infer (TupleRight:path) env e2
		--The inferred types of each side of the tuple must be the same as the assumptions for them
		let constrs = unionConstraints $ 
			[S.fromList [Eq e1Ty e1Assum, Eq e2Ty e2Assum], e1Constrs, e2Constrs]
		return (tupleConstr `TyApp` e1Ty `TyApp` e2Ty, constrs)
-}
	Let ptrn e1 e2 -> do
		(ptrnTy, ptrnConstrs, ptrnBindings) <- inferPattern env ptrn
		(e1Ty, e1Constraints) <- infer (Let1st:path) env e1
		(newEnv, generalized) <- generalize (env `inject` ptrnBindings) e1Ty e1Constraints
		(e2Ty, e2Constraints) <- infer (Let2nd:path) (newEnv `inject` ptrnBindings) e2
		let constraints = 
			unionConstraints $ (S.singleton $ Eq ptrnTy e1Ty):[e1Constraints, e2Constraints, ptrnConstrs]
		
		return (e2Ty, constraints)

	RecordConstr recName fields -> case lookupFromFields (map fst fields) env of

		Nothing -> throwError $ RecordNotFound recName
		Just (recSch, fieldMap) -> do

			--Make sure they are sorted in ascending order
			let nameTyPairs = M.toAscList fieldMap

			--At the moment this is only checking from fields and not from the constructor, need to also change this to
			--check that the constructor tag matches the type aswell
			recTy <- instantiate recSch

			--The expected types
			let (_,expectedTys) = unzip nameTyPairs

			--Pairs of strings and expressions
			let recNamePairs = M.toAscList $ M.fromList fields


			--A list of types paired to constraints found by inferring each of the field definitions given
			results <- mapM (\(name, expr) -> infer (RecField name:path) env expr) recNamePairs
			
			--The actual types
			let fieldTypes = map fst results	
			let constraints = unionConstraints $ [S.fromList $ zipWith Eq expectedTys fieldTypes] ++ (map snd results)

			--Check passed, so all field types are fine
			return (recTy, constraints)

	FieldReplace recVar replacements -> case lookupFromFields (map fst replacements) env of
		Nothing -> throwError $ NotFound recVar
		Just (sch, fieldMap) -> do
			--The expected record type found from looking up the fields
			expectedRecTy <- instantiate sch	
			--Also lookup the record variable to make sure it is the record type aswell
			actualRecTy <- instantiate $ fromJust $ lookup recVar env
			let fieldNamesOrder = map fst replacements
			let replacementExprs = map snd replacements		
			inferenceResults <- mapM (\(expr,i) -> infer (Ind i:path) env expr) (zip replacementExprs [0..])
			let fieldTys = map fst inferenceResults
			let fieldConstrs = map snd inferenceResults
			--Will be returned in the same corresponding field order as the inferred types
			expectedTys <- case lookupAll fieldNamesOrder fieldMap of
				--This should already have been verified by the lookup
				Nothing -> throwError $ Misc "This should not be possible"
				Just tys -> return tys
			let constraints = unionConstraints $
				S.fromList (Eq expectedRecTy actualRecTy:(zipWith Eq fieldTys expectedTys)):fieldConstrs
			return (actualRecTy, constraints)

{-
	--Field replace has the source record instance thats being changed, so then we need to again look this up
	--in the environment to determine the type of the record instance 
	FieldReplace sourceRec replacements -> case lookupFromFields (map fst replacements) env of

		--Make this error more advanced in future to show the fields that were searched for
		Nothing -> throwError $ NotFound sourceRec
		Just (recName, fieldMap) -> do

			--Sort the pairs of names and expressions into ascending order
			let replacementPairs = M.toAscList $ M.fromList replacements
			
			--The field names sorted in ascending order
			let names = map fst replacementPairs

			--A list of pairs of the record field types and the constraints they generated
			fieldTyInfo <- mapM (\(name, expr) -> infer (RecField name:path) env expr) replacementPairs
			
			let fieldSchs = map (fieldMap M.!) names
			instnces <- mapM instantiate fieldSchs

			--Could turn this into a mkConstraintList global function later
			let helper [x] [y] = [Eq x y]; helper (x:xs) (y:ys) = Eq x y:helper xs ys; helper _ _ = []

			let constraints = unionConstraints $ 
				[S.fromList $ helper instnces (map fst fieldTyInfo)] ++ (map snd fieldTyInfo) 
				--We must ensure the record with the fields we are replacing from is the expected type
			

			return (Constr recName Concrete, constraints)
-}

	FieldSelect recExpr field -> case lookupFromFields [field] env of
		Nothing -> throwError $ WrongFields [field]
		Just (sch, fieldMap) -> do
			expectedTy <- instantiate sch
			(recTy, recConstrs) <- infer (RecLeft:path) env recExpr
			fieldTy <- return $ fromJust $ M.lookup field fieldMap
			return (fieldTy, S.union (S.singleton $ Eq recTy expectedTy) recConstrs)

{-

OLD VERSION MOST LIKELY DELETE

	FieldSelect recExpr fieldVar -> case lookupFromFields [fieldVar] env of
		Nothing -> throwError $ NotFound $ recVar
		Just (recName, fieldMap) -> do

			--No constraints are generated, this is basically just a variable lookup but a little bit extra
					--i.e. finding out the type of the source record and then finding out the type of the field associated
					--with the source record type
			let fieldSch = fieldMap M.! fieldVar
			fieldInst <- instantiate fieldSch
			--If we are selecting a field from a record, then we must add a constraint that the variable that we are 
			--selecting from is indeed the record type we are expecting
			return (fieldInst, S.empty)
-}

	--At the moment, list generators are simply lists of integers
	--NEED TO IMPLEMENT THE INTERVAL PART
	ListGen from interval to -> do
		(fromTy, fromConstrs) <- infer (Trivial:path) env from
		(toTy, toConstrs) <- infer (Trivial:path) env from

		--If there is an interval expression then we must also take its constraints into account and state that
		--the interval expression itself must also be an int type
		intervalResults <- case interval of
			Nothing -> return Nothing
			Just expr -> Just <$> infer (Trivial:path) env expr
		let intervalConstrs = case intervalResults of
			Nothing -> S.empty
			Just (intervalTy, intervalConstrs) -> S.union (S.singleton $ Eq intervalTy intTy) intervalConstrs
			
		let constraints = 
			unionConstraints $ intervalConstrs:[S.fromList [Eq fromTy intTy, Eq toTy intTy], fromConstrs, toConstrs]
		return (listConstr `TyApp` intTy, constraints)

{-
Given the current environment, a type that has been inferred and the set of 
constraints that has been produced from inferring it, run unification on the set of 
constraints and apply the resulting substitution to both the type and the environment,
and then quantify only those type variables that appear in the substituted
type and not in the substituted environment, finally return the changed environment
pairs with theh resulting scheme containing the quantified types
-}
generalize :: Environment -> Type -> S.Set Constraint -> InferenceContext (Environment, Scheme)
generalize env ty constraints = do
	let result = doUnification env constraints
	when (isLeft result) (throwError $ Misc $ (\(Left err) -> err) result)
	let Right substitution = result
	let subbed = ty `applySubs` substitution
	let newEnv = env `substEnv` substitution
	let quantVars = [tv | tv@(TyVar name _) <- extractTyVars subbed, not $ (show name) `member` newEnv]
	--Create a mapping of all those variables that can be quantified with their replacement
	--QuantVar data types
	let mapping = zip quantVars (map QuantVar [0..])
	let quantified = Scheme (map getKind quantVars) $ applySubs subbed mapping
	return (newEnv, quantified)