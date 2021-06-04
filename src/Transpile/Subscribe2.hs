module Transpile.Subscribe2 where

import Data.List
import Data.Maybe

import Transpile.Expression
import Transpile.Util

import AST.Definition
import AST.Expression
import AST.Module

data Subscriptions = Subscriptions {
	onTimePassed :: Maybe Subscription,
	onKeyPressed :: Maybe Subscription,
	onKeyReleased :: Maybe Subscription,
	onMouseMoved :: Maybe Subscription
	}

data Subscription 
	--The interval expression, the event expression, the observables and the predicate
	= OnTimePassed Expr Expr [String] Expr
	--the interval key expressoins, the event expression, the observables and the predicates for each
	| OnKeyPressed [Expr] [Expr] [String] [Expr]
	--the interval key expressoins, the event expression, the observables and the predicates for each
	| OnKeyReleased [Expr] [Expr] [String] [Expr]
	--The event expression, the observables and the predicate
	| OnMouseMoved Expr [String] Expr

getObservables :: Subscription -> [String]
getObservables sub = case sub of
	OnTimePassed _ _ obs _ -> obs
	OnKeyPressed _ _ obs _ -> obs
	OnKeyReleased _ _ obs _ -> obs
	OnMouseMoved _ obs _ -> obs

--Oof thats a biggun :/
writeSubscriptions :: Module -> String
writeSubscriptions mod = 
	let
	fieldNames = getStateFieldNames mod
	subDfn = fromJust $ lookupSpecialDfn "subscribe" mod
	subscriptions = extractSubscriptions subDfn fieldNames
	present = map fromJust $ filter isJust $ 
		map (\selector -> selector subscriptions) [onTimePassed, onKeyPressed, onKeyReleased, onMouseMoved]
	writeSubscription sub = 
		let
		writePreds preds = case nub preds of
		--		[pred] -> "(" ++ writeExpr 2 pred mod ++ ")"
			preds -> intercalate " && " (map (\pred -> "(" ++ writeExpr 2 pred mod ++ ")") preds)
		writeKeyHandler keyCharCodes events = 
			"\t\tfunction $handler(e){\n" ++ 
				concatMap writeKeyHandlerCase (zip keyCharCodes events) ++
			"\t\t}\n"
		writeKeyHandlerCase (keyExpr,event) = 
			"\t\t\tif(e.code === $keyExprToCode(" ++ writeExpr 3 keyExpr mod ++ 
				")){$dispatch(" ++ writeExpr 3 event mod ++ ")}\n"
		in case sub of
			OnTimePassed interval event _ pred -> 
				"\t\tlet $time = undefined\n\
				\\t\tif(" ++ writeExpr 2 pred mod ++"){$time = setInterval(() => $dispatch(" ++ 
					writeExpr 2 event mod ++ ")," ++ writeExpr 2 interval mod ++ ")}\n\
				\\t\treturn () => clearInterval($time)\n"
			OnMouseMoved responseHandler _ pred -> 
				"\t\tfunction $handler(e){$dispatch(" ++ writeExpr 2 responseHandler mod ++ "(e.pageY)(e.pageX))}\n\
				\\t\tif(" ++ writeExpr 2 pred mod ++ "){document.addEventListener(\"mousemove\",$handler)}\n\
				\\t\treturn () => document.removeEventListener(\"mousemove\",$handler)\n"
			OnKeyPressed chars events _ preds -> 
				writeKeyHandler chars events ++
				"\t\tif(" ++ writePreds preds ++"){document.addEventListener(\"keydown\",$handler)}\n\
				\\t\treturn () => document.removeEventListener(\"keydown\",$handler)\n"
			--OnKeyReleased chars events obs
			OnKeyReleased chars events _ preds ->
				writeKeyHandler chars events ++
				"\t\tif(" ++ writePreds preds ++"){document.addEventListener(\"keyup\",$handler)}\n\
				\\t\treturn () => document.removeEventListener(\"keyup\",$handler)\n"
	writeUseEffect sub observables = 
		"\tReact.useEffect(() => {\n" ++
		writeSubscription sub ++
		"\t},[" ++ intercalate "," (map (\prop -> "state." ++ prop) observables) ++ "])\n"
	in
	concatMap (\sub -> writeUseEffect sub (getObservables sub)) present

extractSubscriptions :: Dfn -> [String] -> Subscriptions
extractSubscriptions (Subscribe _ _ subCases _) fieldNames = 
	helper (Subscriptions Nothing Nothing Nothing Nothing) $ subCases
	where
		helper subs [] = subs
		helper subs ((sub,pred):cases) = case sub of
			[Var "onTimePassed", interval, event] -> 
				helper (subs {onTimePassed = Just $ OnTimePassed interval event observables pred}) cases
			[Var "onKeyPressed", charCode, event] -> 
				helper (addKeyPressedCase charCode event subs observables pred) cases
			[Var "onKeyReleased", charCode, event] -> 
				helper (addKeyReleasedCase charCode event subs observables pred) cases
			[Var "onMouseMoved", event] -> 
				helper (subs {onMouseMoved = Just $ OnMouseMoved event observables pred}) cases
			where
				observables = extractObservables pred fieldNames 

addKeyPressedCase :: Expr -> Expr -> Subscriptions -> [String] -> Expr -> Subscriptions
addKeyPressedCase char event rc observables pred = case onKeyPressed rc of
	Nothing -> rc {onKeyPressed = Just $ OnKeyPressed [char] [event] observables [pred]}
	Just (OnKeyPressed chars events obs preds)-> 
		rc {onKeyPressed = Just $ OnKeyPressed (char:chars) (event:events) (obs ++ observables) (pred:preds)}

addKeyReleasedCase :: Expr -> Expr -> Subscriptions -> [String] -> Expr -> Subscriptions
addKeyReleasedCase char event rc observables pred = case onKeyReleased rc of
	Nothing -> rc {onKeyReleased = Just $ OnKeyReleased [char] [event] observables [pred]}
	Just (OnKeyReleased chars events obs preds) -> 
		rc {onKeyReleased = Just $ OnKeyReleased (char:chars) (event:events) (obs ++ observables) (pred:preds)}


{-
Given the subscribe block and the parsed module, return lists 
where indices of the lists correspond to the order of the cases,
and the contents of each list is the observable field values that are
being referenced in the predicate expression of that case, so that 
the transpiler knows to put those observable variables into
the dependency array of useEffect
-}
extractCaseObservables :: Dfn -> Module -> [[String]]
extractCaseObservables (Subscribe _ _ subCases _) mod = 
	map (\(_,expr) -> extractObservables expr $ getStateFieldNames mod) subCases

{-
The expression case will only return a singleton list wrapped in a list of the observables in that expression
The expression that is being searched is the right hand side of the "?<<" in the subscribe structure

Unlike before, we DONT have to search the parse tree of all variables that are referenced, BECAUSE
if a helper function is called, then the state field will be given as an argument to it so the observable
variable will be detected when the application expression is examined!
-}
extractObservables :: Expr -> [String] -> [String]
extractObservables expr fieldNames = case expr of
	FieldSelect e field -> extractObservables e fieldNames ++ 
		if field `elem` fieldNames then [field] else []
	OptVar _ (Optional stringExprPairs) -> 
		concatSearch $ map snd stringExprPairs
	Let _ e1 e2 -> concatSearch [e1,e2]
	Tuple es -> concatSearch es
	App e1 e2 -> concatSearch [e1,e2]
	Application e es -> concatSearch $ e:es
	Lambda _ body -> extractObservables body fieldNames
	Match matchee ptrnExprPairs -> let
		matchCaseExprs = map snd ptrnExprPairs
		in
		concatSearch $ matchee:matchCaseExprs
	Conditional predExprPairs -> concatSearch $ 
		(\(preds,exprs) -> preds ++ exprs) $ unzip predExprPairs
	IfEl pred e1 e2 -> concatSearch [pred,e1,e2]
	RecordConstr _ fields -> concatSearch $ map snd fields
	FieldReplace _ fields -> concatSearch $ map snd fields
	--ONLY field selections are the observable properties
	ListGen from interval to -> concatSearch $ 
		[from,to] ++ case interval of
			Nothing -> []
			Just expr -> [expr]
	_ -> []
	where
		concatSearch list = concatMap (\e -> extractObservables e fieldNames) list