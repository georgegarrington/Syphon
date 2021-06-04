{-# LANGUAGE TupleSections #-}
module Parse.Definition where

import Control.Monad.Reader

import Text.Megaparsec hiding (sepBy)
import Text.Megaparsec.Char

import AST.Definition
import AST.Expression
import AST.Type
import AST.Pattern

import Parse.Expression
import Parse.Type
import Parse.Pattern
import Parse.Util
import Parse.Lit

pDfn = pAlias <|> pTypeDfn <|> pSubBlock <|> pStaticConstant <|> pAsset <|> pFun 

pAsset = do
	try $ pKeyword "asset"
	name <- pIdentifier
	path <- inLf $ lChar '=' *> litString
	greedySc
	return $ Asset name path

pStaticConstant :: Parser Dfn
pStaticConstant = noLf $ do
	lineNum <- fmap (unPos . sourceLine) getSourcePos
	--Not sure if this is too much backtracking, seems okay
	identifier <- try $ pIdentifier <* lChar '='
	expr <- pExpr
	--Look at that, parser with integrated type inference :O
	(literal, ty) <- case expr of
		l@(Var "True") -> return (l,boolTy)
		l@(Var "False") -> return (l,boolTy)
		l@IntVal {} -> return (l,intTy)
		l@DoubleVal {} -> return (l,doubleTy)
		l@CharVal {} -> return (l,charTy)
		l@StringVal {} -> return (l,stringTy)
		_ -> simpleErr "Static constants can only be literal Bools, Ints, Doubles, Chars or Strings!"
	greedySc
	return $ Fun identifier lineNum (toScheme ty) [Case lineNum [] literal] []

{-
lets <- label "a where definition" $ incLvlBy 1 $ inLf $ some $ do
				indent
				ptrns <- some pPattern
				body <- inLf $ lChar '=' *> pExpr
				blankLines
				return $ case ptrns of
					[pat] -> Let pat body
					pat:pats -> Let pat (Lambda pats body)
				--partially applied let expression which is in scope of the next where let definition, which in turn is 
				--in scope of the next where let definition and so on, until one large nested let expression has 
				--been built up which is in scope of the body case expression
			greedySc
			return $ nestLets lets
-}

pSubBlock :: Parser Dfn
pSubBlock = noLf $ do
	lineNum <- fmap (unPos . sourceLine) getSourcePos
	try $ char '@' >> pKeyword "subscribe"
	cases <- some pSubCase
	--The wheres stuff can easily be converted into let expressions for the type system later
	wheres <- option [] pWheres
	greedySc
	return $ Subscribe "subscribe" lineNum cases wheres
	where
		pSubCase = label "a subscription case" $ incLvlBy 1 $ do
			try $ indent *> notFollowedBy (pKeyword "where")
			exprs <- some pSimpleExpr
			pred <- inLf $ do
				op "?<<"
				pExpr
			return (exprs,pred)
		pWheres = incLvlBy 1 $ do
			indent
			pKeyword "where"
			incLvlBy 1 $ some $ do
				indent
				ptrns <- some pPattern
				body <- inLf $ lChar '=' *> pExpr
				return (ptrns,body)

pAlias :: Parser Dfn
pAlias = inLf $ do
	lineNum <- fmap (unPos . sourceLine) getSourcePos
	--Once we see this keyword we can lock on to this branch
	try $ pKeyword "alias"
	name <- pCapsIdentifier 
	lChar '='
	ty <- pType
	greedySc
	return $ Alias name lineNum ty

--Either an ADT or a record type
pTypeDfn :: Parser Dfn
pTypeDfn = inLf $ do
	lineNum <- fmap (unPos . sourceLine) getSourcePos
	--The keyword we need to see to lock on to the branch
	try $ pKeyword "type"
	name <- pCapsIdentifier
	lChar '='
	pRecord name lineNum <|> pADT name lineNum
	where
		pRecord name lineNum = do
			lChar '{'
			fields <- sepBy (lChar ',') 
				((,) <$> (pIdentifier <* lString "::") <*> pType)
			lChar '}'
			greedySc
			return $ Record name lineNum fields
		pADT name lineNum = do
			constrs <- sepBy (lChar '|') ((,) <$> pCapsIdentifier <*> many tyAtom)
			greedySc
			return $ ADT name lineNum constrs

pFun :: Parser Dfn
pFun = do
	lineNum <- fmap (unPos . sourceLine) getSourcePos
	--All functions/variable must have a type signature
	--NO INDENTATION SUPPORT AT THE MOMENT, IMPROVE IT SO THAT IT DOES
	name <- try pIdentifier
	sch <- inLf $ lString "::" *> pScheme
	--Consume leading blank lines after type definition
	blankLines
	cases <- some $ pCase name
	--Consume any and all leading space after the function
	greedySc
	--ALSO NO WHERES DEFINITION SUPPORT YET SO IMPLEMENT THAT TOO
	return $ Fun name lineNum sch cases []
	where
		pCase expected = do
			try $ lChar '\n' >> lString expected
			lineNum <- fmap (unPos . sourceLine) getSourcePos
			args <- many pPattern
			body <- (inLf $ lChar '=' *> pExpr) <|> pCond True
			--Consume leading blank lines
			blankLines
			wheres <- option Nothing (Just <$> noLf pWhereDfns)
			return $ case wheres of
				Nothing -> Case lineNum args body
				--The "where" definitions are simply treated as let expressions in scope of the case body expressions
				Just partialLetExpr -> Case lineNum args (partialLetExpr body)

--Returns the where definitions as one large nested let expression that takes a body expression
pWhereDfns = incLvlBy 1 $ do
	--The only part that we need to "lock on" to the where definition branch
	try $ indent >> pKeyword "where"
	blankLines
	lineNum <- fmap (unPos . sourceLine) getSourcePos
	lets <- label "a where definition" $ incLvlBy 1 $ inLf $ some $ do
		indent
		ptrns <- some pPattern
		body <- inLf $ lChar '=' *> pExpr
		blankLines
		return $ case ptrns of
			[pat] -> Let pat body
			pat:pats -> Let pat (Lambda pats body)
		--partially applied let expression which is in scope of the next where let definition, which in turn is 
		--in scope of the next where let definition and so on, until one large nested let expression has 
		--been built up which is in scope of the body case expression
	greedySc
	return $ nestLets lets