module Parse.Pattern (pPattern) where

import Text.Megaparsec hiding (sepBy)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

import AST.Pattern

import Parse.Lit
import Parse.Util

{-
Either a single simple pattern or a cons 
pattern of multiple simple patterns
-}
pPattern :: Parser Pattern
pPattern = label "a pattern" $ do
	ptrns <- sepBy (lChar ':') pSimplePattern
	return $ case ptrns of
		[ptrn] -> ptrn
		_ -> ConsPattern ptrns


	{-}
	pSingleOrCons <?> "a pattern"
	where
		pSingleOrCons = do
			frst <- pSimplePattern
			rest <- many $ lChar ':' *> pSimplePattern
			case rest of
				[] -> return frst
				_ -> return $ ConsPattern $ frst:rest -}

pSimplePattern :: Parser Pattern
pSimplePattern
	= pSugarList
	<|> pVar 
	<|> pWildcard
	<|> (try pDouble)
	<|> pInt
	<|> pString
	<|> pChar
	<|> pConstructor
	<|> pParenthsPattern

--Have to remove pConstructor so it doesnt parse nested constructors
pConstructorArgPtrn :: Parser Pattern
pConstructorArgPtrn = pSugarList
	<|> pVar 
	<|> pWildcard
	<|> (try pDouble)
	<|> pInt
	<|> pString
	<|> pChar
	<|> (VarPattern <$> pCapsIdentifier)
	<|> pParenthsPattern

pConstructor :: Parser Pattern
pConstructor = do
	tag <- pCapsIdentifier
	args <- many pConstructorArgPtrn
	return $ Constructor tag args

--Just a different syntax of writing cons pattern with implicit empty list on the end, so manually add it
pSugarList :: Parser Pattern
pSugarList = do
	try $ lChar '[' --so that this parser does not clash with var [] parser
	contents <- option [] $ sepBy (char ',') pPattern
	lChar ']'
	return $ if null contents then VarPattern "[]" else SugarList contents --ConsPattern $ contents ++ [VarPattern "[]"]

--Either a pattern expression wrapped in parenths or a tuple pattern
pParenthsPattern :: Parser Pattern
pParenthsPattern = do
	lChar '('
	neg <- option False (True <$ lChar '-')
	let negateNumPtrn pat = case pat of
		IntPattern val -> IntPattern $ negate val
		DoublePattern val -> DoublePattern $ negate val
	frst <- fmap (if neg then negateNumPtrn else id) pPattern
	(frst <$ lChar ')') <|> do
		rest <- some (lChar ',' *> pPattern)
		lChar ')'
		return $ TuplePattern $ frst:rest

pWildcard :: Parser Pattern
pWildcard = Wildcard <$ lChar '_'

pInt :: Parser Pattern
pInt = IntPattern <$> litInt

pDouble :: Parser Pattern
pDouble = DoublePattern <$> litDouble

pChar :: Parser Pattern
pChar = CharPattern <$> litChar

pString :: Parser Pattern
pString = StringPattern <$> litString

pVar :: Parser Pattern
pVar = VarPattern <$> pIdentifier