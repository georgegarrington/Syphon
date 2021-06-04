{-# LANGUAGE FlexibleInstances #-}
module TypeCheck.Root where

import Control.Monad.Except

import Data.Maybe
import Data.Either
import qualified Data.Set as S
import qualified Data.Map as M

import AST.Definition
import AST.Module
import AST.Pattern
import AST.Expression
import AST.Type
import AST.Util

import TypeCheck.Common
import TypeCheck.Environment
import TypeCheck.Inference
import TypeCheck.Unification

--Should probably do something with the line numbers later, because the list of errors may be large
--and hard to make sense of
checkModule :: Module -> [Error]
checkModule mod = {-[Misc $ show $ aliases mod] ++ (map Misc $ msgs environment) ++ -} concatMap (checkDfn environment) ((funs mod) ++ (specialDfns mod))
	where
		environment = initEnv mod
	

subWheresToPartialLets :: [([Pattern], Expr)] -> [Expr -> Expr]
subWheresToPartialLets [] = []
subWheresToPartialLets ((ptrns,body):rest) = case ptrns of
	[singleton] -> (Let singleton body):subWheresToPartialLets rest
	(frst:restPtrns) -> (Let frst (Lambda restPtrns body)):subWheresToPartialLets rest

checkDfn :: Environment -> Dfn -> [Error]
{-
checkDfn env (Subscribe lineNum cases) = 
	let
	(exprsList, preds) = unzip cases
	--Should al be subscriptions
	subExprs = map applicationify exprsList
	map ()-}

--To type check a subscribe block, we just treat it as all the where definitions as being
--one huge nested let expression in scope of a conditional :O
checkDfn env (Subscribe _ lineNum cases whrs) = 
	let
	(unApplifieds,preds) = unzip cases
	applifieds = map applicationify unApplifieds
	--partialLet = nestLets $ subWheresToPartialLets whrs
	--finalExpr = partialLet $ Conditional $ zip preds applifieds
	body = Conditional $ zip preds applifieds
	finalExpr = case whrs of
		[] -> body
		_ -> (nestLets $ subWheresToPartialLets whrs) body
	in
	case doInferExpr (bind "state" (toScheme $ state env) env) finalExpr of
		Left err -> [WithLineNum lineNum err]
		Right (unsubbed, constrs) -> case doUnification env constrs of
			Left errMsg -> [WithLineNum lineNum $ Misc errMsg]
			Right subs -> case unsubbed `applySubs` subs of
				subscriptionTy -> []
				wrong -> [WithLineNum lineNum $ Misc $ "Subscribe block is not a subscription type, it is: " ++ show wrong ++ "!!"] 

checkDfn env (Fun funName lineNum scheme cases' wheres) = 
	let
	inferenceResults = map (\cse@(Case lineNum _ _) -> (doInferCase env funName cse,lineNum)) cases'
	inferenceErrors = map (\(Left x,lineNum) -> WithLineNum lineNum x) $ filter (\(x,_) -> isLeft x) inferenceResults
	unsubbedTyConstrs = map (\(Right (tys,constrs),lineNum) -> (tys,constrs,lineNum)) $ filter (\(x,_) -> isRight x) inferenceResults
	unificationResults = map (\(_,constrs,lineNum) -> (lineNum,doUnification env constrs)) unsubbedTyConstrs
	--Change the unification errors to not just be strings later
	unificationErrors =  map (\(lineNum,Left x) -> WithLineNum lineNum $ Misc x) $ filter (\(_,x) -> isLeft x) unificationResults
	substitutions = map (\(_,Right x) -> x) $ filter (\(_,x) -> isRight x) unificationResults
	unsubbedTys = map (\(x,_,_) -> x) unsubbedTyConstrs
	subbedTys = map (\(ty, substitution) -> ty `applySubs` substitution) (zip unsubbedTys substitutions)
	allSame = and $ map (== (head subbedTys)) (drop 1 subbedTys) 
	--For now unification errors are just strings, should be changed later
	in
	inferenceErrors ++ (unificationErrors) ++ if allSame then [] else [CaseMismatch]