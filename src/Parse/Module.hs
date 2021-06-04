module Parse.Module where

import Text.Megaparsec
import Text.Megaparsec.Char

import AST.Module
import AST.Definition

import Parse.Definition
import Parse.Util

import Misc.Favourites

type Jumbled = (String, [String], [Import], [Dfn])

--All definitions can be given in any order so they will be "jumbled up"
pJumbled :: Parser Jumbled
pJumbled = do
	(name, path) <- option ("",[]) $ do
		greedyLexeme $ pKeyword "module"
		frst <- pCapsIdentifier
		--If there was no "rest" then there is no module path
		rest <- option [] (some $ char '.' *> pCapsIdentifier)
		greedySc
		greedyLexeme $ pKeyword "where"
		let name = case rest of
			[] -> name
			_ -> last rest
		let path = case rest of
			[] -> []
			_ -> [frst] ++ (snip 1 rest)
		return (name, path)
	imports <- pImports
	dfns <- many pDfn
	return (name, path, imports, dfns)

--The grammar of imports is essentially context free so we can just use a greedy space consumer
pImports :: Parser [Import]
pImports = many $ do
	lexeme $ pKeyword "import"
	frst <- pCapsIdentifier
	--If there was no "rest" then there is no module path
	rest <- option [] (some $ char '.' *> pCapsIdentifier)
	greedySc
	--If there is no alias then use empty string
	alias <- option "" $ (greedyLexeme $ pKeyword "as") *> greedyLexeme pCapsIdentifier
	hidden <- option [] $ do
		lexeme $ pKeyword "hiding"
		lexeme $ char '('
		frst <- lexeme $ (pIdentifier <|> pCapsIdentifier)
		rest <- many $ greedyLexeme (char ',') *> greedyLexeme (pIdentifier <|> pCapsIdentifier)
		lexeme $ char ')' 
		return $ frst:rest
	{-
	If there is e.g. Control.Monad.Reader then the module path is ["Control", "Monad"] and the name is
	"Reader", but if there is no module path then the path is empty and the name is the first thing
	parsed 
	-}
	let name = case rest of
		[] -> name
		_ -> last rest
	let modulePath = case rest of
		[] -> []
		_ -> [frst] ++ (snip 1 rest)
	return $ Import rest frst alias hidden