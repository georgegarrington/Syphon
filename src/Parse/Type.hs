module Parse.Type where

import Text.Megaparsec hiding (sepBy)
import Text.Megaparsec.Char

import AST.Type

import Parse.Util

pType = digitify . arrowify <$> sepBy (lString "->") tyApp

pScheme = do
	tyUnits <- sepBy (lString "->") tyApp
	let kinds = replicate (length tyUnits) Concrete
	return $ Scheme kinds $ digitify $ arrowify $ tyUnits

--Some type atom applied to other type atoms, the first type atom must be a constructor
tyApp = do
	tys <- some tyAtom
	let n = length tys
	case tys of
		[ty] -> return ty
		--The number of types that a type constructor takes is clearly its kind
		(Constr name _:tys) -> return $ construct $ (Constr name $ mkKind n):tys
		--Anything else should not be possible

tyAtom 
	= listTy
	<|> parenthsTy
	<|> var
	<|> constr

--Type variables must be single lowercase characters
var = lexeme $ ParsedQuant <$> lowerChar <* notFollowedBy lowerChar

--Constructors must be capitalised
constr :: Parser Type
constr = do
	name <- pCapsIdentifier
	return $ Constr name Concrete

--List type constructor applied to some type
listTy = betweenChars '[' ']' $ (listConstr `TyApp`) <$> pType

--Either the unit type, a normal type wrapped in parenths or a tuple type
parenthsTy :: Parser Type
parenthsTy = inParenths $ do
	tys <- option [] $ sepBy (char ',') pType
	return $ case tys of
		[] -> unitTy
		[ty] -> ty
		tys -> mkTuple tys