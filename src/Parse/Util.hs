module Parse.Util where

import Prelude hiding (log, init, fail)

import Control.Monad.Reader
import Control.Monad.State.Strict

import Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Text.Show.Pretty

--The parser context
type Parser = ParsecT ParseErr String (Reader ParseEnv)

--The environment the parser works in
data ParseEnv = ParseEnv {tabLvl :: Int, inLineFold :: Bool}

--My custom error type
data ParseErr = CustomErr String deriving (Show, Eq, Ord)

data Thingy = Thing1 | Thing2

--Make sure my custom error is printable
instance ShowErrorComponent ParseErr where
    showErrorComponent (CustomErr s) = s

--The file to parse as a string, which parser to parse, an initial enivonment to run in and return wither error string or parsed thingy
execParser :: String -> Parser a -> ParseEnv -> Either String a
execParser source parser env = 
	case runReader (runParserT (greedySc *> parser <* eof) "" source) env of
		Left err -> Left $ errorBundlePretty err
		Right thing -> Right thing

comment :: Parser ()
comment = L.skipLineComment "--"

commentMl :: Parser ()
commentMl = L.skipBlockComment "{-" "-}"

--Simply run the parser in an environment where the indentation level is incremented by 1, essentially a line fold
indent1Block :: Parser a -> Parser a
indent1Block p = local (\env -> env {tabLvl = tabLvl env + 1}) p

--Similar to line fold but we can specify how many levels we want to 
indentByBlock :: Int -> Parser a -> Parser a
indentByBlock n p = local (\env -> env {tabLvl = tabLvl env + n}) p 

--Consumes either normal space or a "linefold" space dictated by the current indent level
indentBlockSpace :: Parser ()
indentBlockSpace = void $ many (char ' ') >> (try $ do
	--the amount of tabs that should be consumed
	char '\n'
	lvl <- asks tabLvl
	count lvl (char '\t')
	many $ char ' ' --don't need to do notFollowedBy \t as the only thing that can come next is spaces
	)

indentSc :: Parser ()
indentSc = do
	inlineSc --maybe there is space on this line
	pBlankLines --maybe there are some blank lines
	--Optional extra bit, "linefold" indent space
	try $ do
		char '\n'
		lvl <- asks tabLvl
		count lvl (char '\t')
		inlineSc

pBlankLines :: Parser ()
pBlankLines = void $ many $ try $ do
	char '\n'
	inlineSc
	lookAhead $ choice [void $ char '\n', eof]

--Only consumes spaces
inlineSc :: Parser ()
inlineSc = L.space (void $ choice [char ' ',char '\160']) comment commentMl

--sc = L.space (void $ char ' ') comment commentMl

normalSc = L.space (void $ choice [char ' ',char '\160']) comment commentMl

--The most used space consumer that is used within line folds
--hidden at the moment as it improves error messaged, maybe unhide it later
sc :: Parser ()
sc = hidden $ do
	let normalSc = L.space (void $ choice [char ' ', char '\160']) comment commentMl
	lf <- asks inLineFold
	if lf 
		then normalSc *> (option () lineFoldSpace) <* normalSc
		else normalSc

blankLines :: Parser ()
blankLines = void (many $ try blankLine) >> sc
	where
		blankLine = label "a blank line" $ do
			char '\n'
			inlineSc
			lookAhead $ choice [void $ char '\n', eof] 

lineFoldSpace :: Parser ()
lineFoldSpace = void $ do
	notFollowedBy $ char '\n' *> char '\t' *> pKeyword "where"
	--Elements in a linefold must have one level of indentation greater
	lvl <- asks tabLvl
	try (char '\n' >> count (lvl + 1) (char '\t'))
	--charAhead <- option False $ (True <$ lookAhead (char '\t'))
	--when charAhead $ simpleErr "Wrong indentation level"
--	option () $ lookAhead (char '\t') >> 
--			(simpleErr $ "Wrong indentation level!")

lexeme :: Parser a -> Parser a
lexeme p = L.lexeme sc p 

lChar :: Char -> Parser Char
lChar chr = lexeme $ char chr

--Consumes any kind of space character except tabs
greedySc :: Parser ()
greedySc = L.space (void $ some (char ' ' <|> char '\n' <|> char '\160')) comment commentMl

greedyLexeme :: Parser a -> Parser a
greedyLexeme p = L.lexeme greedySc p

--Consume leading space inside the parentheses
inParenths :: Parser a -> Parser a 
inParenths p = betweenChars '(' ')' (sc *> p)

--Consume leading space
betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars fst lst p = lexeme $ between (char fst) (char lst) (sc *> p)

--ASSUME THAT EVERY PARSER HANDLES ITS TRAILING WHITESPACE

--Only one element is required in the seperated list, so no seperators appearing then
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = do
	sc
	fst <- p
	rest <- many $ sep *> sc *> p
	return $ fst:rest

--At least two elements are required in the seperated list (i.e. at least one seperator appears)
sepBySome :: Parser a -> Parser b -> Parser [b]
sepBySome sep p = do
	sc
	fst <- p
	rest <- some $ sep *> sc *> p
	return $ fst:rest

--Parser a specific keyword
pKeyword :: String -> Parser ()
pKeyword kw = void $ lexeme $ string kw <* notFollowedBy alphaNumChar

opChars = "!@#$%^&*-=_+;<>?/:|"

--Some valid binary operator string
opStr :: Parser String
opStr = label "a binary operator string" $ try $ lexeme ((some $ oneOf opChars) <* notFollowedBy (oneOf opChars))

--Parse a specific binary operator
op :: String -> Parser String
op op' = do
	binOp <- try $ lexeme (string op' <* notFollowedBy (oneOf opChars))
	when (not $ and $ map (\char -> char `elem` opChars) op') (simpleErr $ "Invalid binary operator: " ++ op')
	return binOp

lString :: String -> Parser ()
lString str = void $ lexeme $ string str <* notFollowedBy alphaNumChar

doubleDot :: Parser ()
doubleDot = void $ lexeme $ string ".." <* notFollowedBy (char '.')

--Capitalised identifier
pCapsIdentifier :: Parser String
pCapsIdentifier = label "a capitalised identifier" $ do
	str <- (:) <$> upperChar <*> many alphaNumChar 
	notFollowedBy alphaNumChar
	sc
	return str

--Function/variable identifier starting with a lowercase char
pIdentifier :: Parser String
pIdentifier = label "an identifier starting with a lowercase char" $ do
	notFollowedBy $ choice $ map pKeyword 
		["if", "else", "then", "match", "with", "where", "let", "in", "cond","pipeline", "is", "shouldFailWith", "shouldReturn", "shouldSatisfy"] 
	str <- (:) <$> lowerChar <*> many alphaNumChar <* notFollowedBy alphaNumChar
	sc
	return str

simpleErr :: String -> Parser a
simpleErr msg = customFailure $ CustomErr msg

--Function used for modifying the reader environment
incLvl :: ParseEnv -> ParseEnv
incLvl env = env {tabLvl = (tabLvl env) + 1}

--Parses the given parser where the reader environment has the indentation level incremented by the specific amount
incLvlBy :: Int -> Parser a -> Parser a
incLvlBy increment p = local (\env -> env {tabLvl = (tabLvl env) + increment}) p 

setLineFold :: Bool -> ParseEnv -> ParseEnv
setLineFold b env = env {inLineFold = True}

lfTrue :: ParseEnv -> ParseEnv
lfTrue env = env {inLineFold = True}

lfFalse :: ParseEnv -> ParseEnv
lfFalse env = env {inLineFold = False}

inLf :: Parser a -> Parser a
inLf p = local lfTrue p

noLf :: Parser a -> Parser a
noLf p = local lfFalse p

indent :: Parser ()
indent = void $ do
	lvl <- asks tabLvl
	try $ do
		char '\n'
		count lvl $ char '\t'
		--Comment this back out if it fails
		option () $ (char '\t') >> (fail $ "Wrong indentation level!")
	sc