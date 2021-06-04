module Parse.SUnit where

import Control.Monad

import Data.List.Split hiding (sepBy)

import AST.Expression
import AST.SUnit

import Text.Megaparsec hiding (sepBy)
import Text.Megaparsec.Char

import Parse.Util
import Parse.Expression
import Parse.Pattern
import Parse.Lit

import Misc.Favourites

--Given the contents of the SUnit file as a string, parse it and return an SUnit module
parseSUnit :: String -> Either String SUnitMod
parseSUnit fileStr = execParser fileStr pSUnitModule (ParseEnv 0 False)

pSUnitModule = do
	greedySc
	--lString "testing"
	--modName <- inParenths pCapsIdentifier
	greedySc
	some (pGlobalLet <|> pFnTests)

pGlobalLet = do
	pKeyword "let"
	ptrn <- pPattern
	inLf $ do
		lChar '='
		expr <- pExpr
		greedySc
		return $ GlobalLet ptrn expr

pLocalLet = incLvlBy 1 $ do
	try $ lChar '\t' *> pKeyword "let"
	ptrn <- pPattern
	inLf $ do
		lChar '='
		expr <- pExpr
		greedySc
		return $ LocalLet ptrn expr


pFnTests = do
	lChar '#'
	name <- some $ noneOf " \n" --So that we can allow for $ prepended functions
	greedySc
	cases <- verboseDfns--((notFollowedBy (char '\t' *> pKeyword "inputs" *> lChar '=')) *> conciseDfns) <|> verboseDfns	
	return $ Test name (concat cases)
	where
		backslashQuotes chars = case chars of
			'\"':rest -> '\\':'\"':backslashQuotes rest
			c:rest -> c:backslashQuotes rest
			_ -> ""
		fixBackslashes chars = case chars of
			'\\':rest -> '\\':'\\':fixBackslashes rest
			c:rest -> c:fixBackslashes rest
			_ -> ""
		command = choice [pKeyword "shouldFailWith", pKeyword "shouldReturn", pKeyword "shouldSatisfy"]
		verboseDfns = some $ do
			lets <- option [] $ some pLocalLet
			char '\t'
			pKeyword "args"
			lChar ':'
			prettyArgs <- fmap trim $ lookAhead $ manyTill anySingle (choice [eof, void $ char '\n'])
			let finalArgs = backslashQuotes $ fixBackslashes prettyArgs
			exprs <- some pSimpleExpr
			greedySc
			char '\t'
			prettyPtrn <- lookAhead $ command *> manyTill anySingle (choice [eof, void (char '\n')])
			let checkedPtrn = if (and $ map (== ' ') prettyPtrn) || null prettyPtrn then "#failure" else prettyPtrn
			let finalPtrn = backslashQuotes checkedPtrn
			expcted <- 
				(Failure <$> 
					((try (pKeyword "shouldFailWith" *> inLf (incLvlBy 1 $ lChar ':'))) *> inLf litString)) 
				<|> (ReturnPtrn <$> (((try $ pKeyword "shouldReturn") *> (inLf (incLvlBy 1 (lChar ':' *> pPattern))))))
				<|> (Predicate <$> ((pKeyword "shouldSatisfy") *> (inLf $ incLvlBy 1 (lChar ':' *> pExpr))))
			greedySc
			return $ lets ++ [TestCase finalArgs finalPtrn exprs expcted]
			{-}
		conciseDfns = some $ do
			char '\t'
			inlineSc
			prettyArgs <- fmap trim $ lookAhead $ some $ notFollowedBy command *> anySingle
			let finalArgs = backslashQuotes $ fixBackslashes prettyArgs
			exprs <- some pSimpleExpr
			prettyPtrn <- lookAhead $ command *> manyTill anySingle (choice [eof, void (char '\n')])
			let checkedPtrn = if (and $ map (== ' ') prettyPtrn) || null prettyPtrn then "#failure" else prettyPtrn
			let finalPtrn = backslashQuotes checkedPtrn
			--testingFr <- (Failure <$ pKeyword "shouldFailWith") <|> (Success <$ pKeyword "shouldReturn")
			--expcted <- option Nothing (Just <$> pPattern)
			expcted <- (Left <$> ((try $ pKeyword "shouldFailWith") *> litString)) 
				<|> (Right <$> (pKeyword "shouldReturn" *> pPattern))
			greedySc
			return $ TestCase finalArgs finalPtrn exprs expcted --PUT STRING FORMS HERE-}
	