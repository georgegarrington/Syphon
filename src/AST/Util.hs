module AST.Util where

import AST.Expression

{-
Given a function expression and the list of expressions that it is
applied to obtained from parsing, turn them into an application data type
-}
--COULD PROBABLY DO WITH CHANGING LATER
applicationify :: [Expr] -> Expr 
applicationify exprs = helper $ reverse $ exprs
	where
		helper [fstArg, fun] = App fun fstArg
		helper (back:reversed) = App (helper reversed) back
		--MAKE THIS NICER
		helper _ = error "I think you tried write a function expression without any arguments!\nCan you see an \
			\optional structure anywhere without any arguments following it?\nThis is most often the cause."
	
