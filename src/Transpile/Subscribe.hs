{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE FlexibleContexts #-}
module Transpile.Subscribe where

import AST.Type
import AST.Definition
import AST.Expression
import AST.Module

import Data.Maybe
import Data.List

import qualified Data.Map as M 
import qualified Data.Set as S

{-
All properties that can be subscribed to, mapped to functions that handle them and their return handling function
e.g. onPassed, clearInterval used so that we can look through the parse tree of the subscribe
function and see all the possible subscribable properties that the program is using and create variables and 
return handling functions for it appropriately
-}
subPropertyFns :: M.Map String (String, S.Set String)
subPropertyFns 
	= M.fromList [
	("$time", ("clearInterval", S.singleton "onPassed")),
	("$null", ("", S.singleton "NoSub"))
	]

--Module level functions that must be present if any of the subscriptions are used
fnDfns :: M.Map String String
fnDfns = M.fromList [("onPassed", onPassedFn)]

onPassedFn = 
	"const onPassed = interval => event => {\n\
	\\t$time = setInterval(() => dispatchRef(event), interval)\n\
	\}\n"

--Query if a function is a subscription generating function
querySub :: String -> Maybe (String, String)
querySub s = helper (M.toList subPropertyFns)
	where
		helper [] = Nothing
		helper ((property, (handler,set)):maps)
			| s `S.member` set = Just (property, handler)
			| otherwise = helper maps

lookupHandler :: String -> String
lookupHandler target = case M.lookup target subPropertyFns of
	Just (handler, _) -> handler
	--Should technically never happen
	Nothing -> error "Subscribable property not found!"

{-}
--Given a subscribable property, find its handler function
lookupHandler :: String -> String
lookupHandler target = helper (M.toList subPropertyFns) target
	where
		--Should technically never happen
		helper [] target = error $ "Subscribable property not found: " ++ target "!"
		helper ((prop, (handler, _)):rest) target
			| prop == target = handler
			| otherwise = helper rest target-}

joinTriplets :: (Eq a, Eq b, Eq c) => ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
joinTriplets (fst1,snd1,trd1) (fst2,snd2,trd2) = (nub $ fst1 ++ fst2, nub $ snd1 ++ snd2, nub $ trd1 ++ trd2)

--Extract all the subscribable properties that are references along with the functions that manipulate them
--We just concat all the results into one large list as all properties are prepended with $ so we can differentiate them this way
--Return a triples of list of subscribable properties, list of subscribe functions used and also list of subscribe handling
--functions that must be placed at the end of the useEffect hook in the view function definition
class SubProps a where extractSubProps :: Module -> a -> ([String],[String],[String])
instance SubProps Module where 
	extractSubProps mod self = 
		foldl1 joinTriplets $ map (extractSubProps mod) $ (fromJust $ lookupSpecialDfn "subscribe" mod):funs mod 
instance SubProps Dfn where extractSubProps mod (Fun _ _ _ cases _) = foldl1 joinTriplets $ map (extractSubProps mod) cases
instance SubProps Case where extractSubProps mod (Case _ _ body) = extractSubProps mod body
instance SubProps Expr where
	extractSubProps mod expr = case expr of
		Var candidate -> case querySub candidate of
			--Even if a variable is not a subscription generating function, we must also check the variables own
			--parse tree to see if it contains any subscription generating functions
			Nothing -> case lookupDfn candidate mod of
				--If we don't find a parse tree for a definition then it is a built in library function and none of those
				--manipulate subscriptions
				Nothing -> ([],[],[])
				Just userDfn -> extractSubProps mod userDfn
			--This variable has been found to be a subscription generating function
			Just (property, handler) -> ([property],[candidate],[handler])
		Optional stringExprPairs -> foldl1 joinTriplets $ map (extractSubProps mod) $ map snd stringExprPairs
		Let ptrn e1 e2 -> extractSubProps mod e1 `joinTriplets` extractSubProps mod e2
		Tuple exprs -> foldl1 joinTriplets $ map (extractSubProps mod) exprs
		App e1 e2 -> extractSubProps mod e1 `joinTriplets` extractSubProps mod e2
		Lambda _ body -> extractSubProps mod body
		Match matchee patExprPairs -> foldl1 joinTriplets $ map (extractSubProps mod) $ matchee:map snd patExprPairs
		Conditional exprExprPairs -> let (preds, exprs) = unzip exprExprPairs in
			(foldl1 joinTriplets $ map (extractSubProps mod) preds) `joinTriplets` (foldl1 joinTriplets $ map (extractSubProps mod) exprs)
		IfEl pred e1 e2 -> foldl1 joinTriplets $ map (extractSubProps mod) [pred, e1, e2]
		RecordConstr _ strExprPairs -> foldl1 joinTriplets $ map (extractSubProps mod) $ map snd strExprPairs
		FieldReplace _ strExprPairs -> foldl1 joinTriplets $ map (extractSubProps mod) $ map snd strExprPairs
		_ -> ([],[],[])