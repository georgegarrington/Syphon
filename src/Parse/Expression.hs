module Parse.Expression where

import Control.Monad.Reader

import Text.Megaparsec hiding (sepBy)
import Text.Megaparsec.Char

import AST.Expression
import AST.Pattern
import AST.Util

import Parse.Pattern
import Parse.Lit
import Parse.Util
import Control.Monad.Combinators.Expr

--This is the one that should pretty much always be used, includes e.g. binary operators and function application too
pExpr :: Parser Expr
pExpr = makeExprParser root opTable

{-
Megaparsec utility, list binary operators in order of descending "tightness" and how they associate,
operators in the same inner list have equal tightness and the inner lists are ordered such that the 
operators in an inner list all bind tighter than the operators in the inner list after them
-}
opTable :: [[Operator Parser Expr]]
opTable = [
	-- $ and : are right associative as in Haskell, all other operators are left associative
	-- Dollar operator will only ever be called with a function call as LHS arg, so 
	--simply add RHS arg to the LHS funcall list of args
	[binary "*", binary "^", binary "/", binary "%"],
	[binary "+", binary "-"],
	[binary "==", binary "!="],
	[binary ">", binary ">=", binary "<", binary "<="],
	[binary "&&"],
	[binary "||"],
	[binary "++"],
	[binary "!!"],
	[InfixR $ handleDollar <$ (op "$" <?> "a binary operator")],
	[InfixR $ (\lhs rhs -> Application (Var ":") [lhs,rhs]) <$ (op ":" <?> "a binary operator")]
	{-
	Parse a custom binary operator that the user has defined, bind the name to the argument "id" and then lift the
	expression making function into a parser context
	-}
	--[InfixL $ op >>= \id -> return $ \lhs rhs -> App (App (Var id) lhs) rhs
	]
	where
		binary op' = InfixL $ (\lhs rhs -> Application (Var op') [lhs,rhs]) <$ ((lexeme $ (notFollowedBy $ op "?<<") *> op op') <?> "a binary operator")
		handleDollar lhs rhs = Application lhs [rhs]--App lhs rhs

--I don't think the order matters that much, cos every sub parser takes care of their initial "locking on" bit :)
--Either a normal simple expression, or a simple normal expression "." something (i.e. field selection)
pSimpleExpr = do
	normalPart <- normal
	fieldSelection <- option "" $ try $ (lChar '.') *> pIdentifier
	patternCheck <- option Nothing $ Just <$> ((try $ pKeyword "is") *> pPattern)
	let normalExprOrFieldSelect = case fieldSelection of
		--No field selection
		"" -> normalPart
		field -> FieldSelect normalPart fieldSelection
	return $ case patternCheck of
		Nothing -> normalExprOrFieldSelect
		Just pat -> PatternCheck normalExprOrFieldSelect pat
	where
		normal = 
			pRecConstr
				<|> pOpt
				<|> pFieldReplace
				<|> pMatch
				<|> ((try $ noLf $ pKeyword "cond") *> pCond False)
				<|> pLet
				<|> pPipeline
				<|> pIf
				<|> pLambda
				<|> pVar
				<|> pInt
				<|> pDouble
				<|> pChar
				<|> pString
				<|> parenthsExpr
				<|> pList
				<?> "a simple expression"

pVar = Var <$> (pIdentifier <|> pCapsIdentifier) <?> "a variable identifier"
pInt = IntVal <$> litInt <?> "a literal int"
pDouble = DoubleVal <$> litDouble <?> "a literal double"
pChar = CharVal <$> litChar <?> "a literal char"
pString = StringVal <$> litString <?> "a literal string"

--Should deffo be moved later
mkCons :: [Expr] -> Expr
mkCons elems = case elems of
	--If there is only a single element then just wrap it in a list
	[e] -> 	Application (Var ":") [e, Var "[]"]
	es -> helper es
	where
		helper [e] = e
		helper (e:es) = Application (Var ":") [e,helper es]

--The root expression is either one expression (not a function) or more than one expression (a function
--, i.e. curried application of several expressions)
root :: Parser Expr
root = local lfTrue $ do
	frst <- pSimpleExpr 
	inlineArgs <- many pSimpleExpr
	multilineArgs <- option [] $ noLf $ do
		--The only thing that we need to "lock on" to the multiline argument branch
		try $ lString "$<<" <* notFollowedBy (char '<')
		local incLvl $ some pMultilineElem
	let args = inlineArgs ++ multilineArgs
	return $ case frst:args of
		--The only place an optional structure should ever appear, 
		--if it appears anywhere else the type checker will throw a bitchfit
		(Var name:opt@(Optional {}):rest) -> case rest of 
			[] -> OptVar name opt
			_ -> Application (OptVar name opt) rest
		[Var "error", msgExpr] -> Error msgExpr
		[e] -> e
		(e:es) -> Application e es

pOpt = do
	lChar '#'
	fields <- betweenChars '{' '}' $ sepBy (char ',') pField
	return $ Optional fields

pRecConstr = do
	--try, so that we don't clash with normal caps identifiers which normal constructors use
	--This is all we need to "lock on" to a record constructor branch as we know that we are in
	--a record constructor once we have seen a caps identifier and a '{'
	constr <- try (pCapsIdentifier <* lChar '{')
	fields <- sepBy (char ',') pField
	lChar '}'
	return $ RecordConstr constr fields

pFieldReplace = betweenChars '{' '}' $ do
	source <- pIdentifier <?> "source record variable"
	lChar '|'
	replacements <- label "record replacement field name/expression pairs" $ sepBy (char ',') pField
	return $ FieldReplace source replacements

--Records/optionals use for assigning expressions to fields
pField = do
	field <- pIdentifier
	lChar '='
	expr <- pExpr
	return (field, expr)
	<?> "a field name/expression pair"

--handles negated integers aswell
--Either the unit value, a normal expression in parentheses, a binary operator or a tuple expression
parenthsExpr = inParenths $ do
	exprs <- option [] $ (sepBy (char ',') pExpr) 
		<|> ((try $ lChar '-' >> litInt) >>= \val -> return [IntVal $ negate val]) 
		<|> ((try $ lChar '-' >> litDouble) >>= \val -> return [DoubleVal $ negate val]) 
		<|> (opStr >>= \str -> return [Var str])
	return $ case exprs of
		[] -> Var "()"
		[expr] -> expr
		es -> Tuple es

--Handles all possible cases for a list, i.e. empty/multiline/normal/generator
pList :: Parser Expr
pList = inLf $ do
	lChar '['
	let elems = sepBy (char ',') pExpr <?> "a comma seperated list of expressions"
	let multilineList = noLf $ do
		try $ lString "]<<" <* notFollowedBy (char '<')
		elms <- local incLvl $ some pMultilineElem
		return $ mkCons $ elms ++ [Var "[]"]
	--Either a multiline list, an empty list or an inline list/generator
	multilineList  <|> (Var "[]" <$ lChar ']') <|> do
		frstPart <- elems
		--Either the end of a normal list or theres a list generator
		(mkCons (frstPart ++ [Var "[]"]) <$ lChar ']') <|> do
			doubleDot
			endExpr <- pExpr
			lChar ']'
			return $ case frstPart of
				--List generator with no interval specified i.e. [1..10]
				[x] -> ListGen x Nothing endExpr
				--List generator with an interval specified i.e. [1,3..10]
				x1:x2:_ -> ListGen x1 (Just x2) endExpr

--For parsing multiline elements that are normal expressions
pMultilineElem = 
	(notFollowedBy $ lChar '\n' >> lChar '\t' >> pKeyword "where") *> (try indent) *> inLf pExpr 
	<?> "a multiline structure element"

--Again, lambdas are inline
pLambda = inLf $ do
	lChar '\\'
	ptrns <- some pPattern <?> "lambda expression argument patterns"
	op "->"
	body <- pExpr <?> "a lambda body expression"
	return $ Lambda ptrns body

--if expressions are inline only
pIf = do
	try $ pKeyword "if"
	pred <- pExpr <?> "an if expression predicate"
	pKeyword "then"
	e1 <- label "the first if branch expression" $ pExpr
	pKeyword "else"
	e2 <- pExpr <?> "the second if branch expression"
	return $ IfEl pred e1 e2

pLet = noLf $ do
	try $ pKeyword "let"
	blankLines
	incLvlBy 1 $ do
		dfns <- some pLetDfn
		indent
		pKeyword "in"
		blankLines
		indent
		body <- inLf pExpr
		blankLines
		return $ (nestLets dfns) body
		
--Change this to be a where definition after testing
pLetDfn = label "a let expression definition" $ do
	try $ indent >> notFollowedBy (choice [pKeyword "in", spaceStart]) 
	ptrns <- some pPattern
	body <- inLf $ lChar '=' *> pExpr
	blankLines
	return $ case ptrns of
		[pat] -> Let pat body
		pat:pats -> Let pat $ Lambda pats body

{-}
pLet = (try $ pKeyword "let") *> noLf $ local incLvl $ do
	indent
	pKeyword "in"
	indent
	pExpr -}

pPipeline = noLf $ do
	try $ pKeyword "pipeline"
	blankLines
	exprs <- local incLvl $ some pPipelineExpr
	return $ mkPipeline $ reverse exprs
	where
		pPipelineExpr = do
			notFollowedBy $ lChar '\n' >> lChar '\t' >> pKeyword "where"
			try $ indent >> notFollowedBy spaceStart
			expr <- inLf pExpr
			blankLines
			return expr
		mkPipeline [expr] = expr
		mkPipeline (e:es) = App e (mkPipeline es)

pCond isDfn = noLf $ do
	blankLines
	cases <- label "conditional expression cases" $ local incLvl $ some pCondCase
	return $ Conditional cases
	where
		pCondCase = do
			notFollowedBy $ lChar '\n' >> lChar '\t' >> pKeyword "where"
			--Hmmm not sure if this is backtracking too much yet. Just wait and see. 
			--Test some pattern labels in match expressions to see if they show up
			pred <- label "a conditional expression predicate" $ indent *> lChar '|' *> pExpr
			inLf $ do
				if isDfn then lString "=" else lString "->"
				inlineSc
				expr <- label "a conditional expression case sub-expression" $ pExpr
				blankLines
				return (pred,expr)

pMatch = noLf $ do
	try $ pKeyword "match"
	matchee <- pExpr <?> "matchee sub-expression to pattern match on"
	pKeyword "with"
	blankLines
	cases <- label "match expression cases" $ local incLvl $ some pMatchCase
	return $ Match matchee cases
	where
		pMatchCase = do
			notFollowedBy $ lChar '\n' >> lChar '\t' >> pKeyword "where"
			--Hmmm not sure if this is backtracking too much yet. Just wait and see
			ptrn <- label "match expression pattern" $ do
				try $ indent *> notFollowedBy spaceStart
				pPattern
			inLf $ do
				op "->"
				inlineSc
				expr <- label "match expression case sub-expression" $ pExpr
				blankLines
				return (ptrn, expr)

spaceStart :: Parser ()
spaceStart = choice [void $ string "--", void $ string "{-", void $ char ' ']