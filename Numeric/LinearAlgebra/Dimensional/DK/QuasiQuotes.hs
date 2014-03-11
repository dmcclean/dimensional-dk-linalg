{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
-- | a variation on <http://hackage.haskell.org/package/hmatrix-syntax>,
-- which constructs a matrix which has dimensions stored.
module Numeric.LinearAlgebra.Dimensional.DK.QuasiQuotes
(
  vecD,
  parse,
  identifierList
) where

import Language.Haskell.TH

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote

import Numeric.Units.Dimensional.DK (Quantity, Dimension, DLength, DMass)
import Numeric.LinearAlgebra.Dimensional.DK.Internal (DimMat(..))
import Numeric.LinearAlgebra.Dimensional.DK.Shapes

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.String
import Text.Parsec.Language (haskellDef)


vecD = QuasiQuoter {
  quoteExp = error "vecD",
  quotePat = error "vecD",
  quoteDec = error "vecD",
  quoteType = parseVectorType
}

vecShapeD = QuasiQuoter {
  quoteExp = error "vecShapeD",
  quotePat = error "vecShapeD",
  quoteDec = error "vecShapeD",
  quoteType = parseVectorShapeType
}

parseVectorType :: String -> Q Type
parseVectorType s = [t| DimMat $(parseVectorShapeType s) |]

parseVectorShapeType :: String -> Q Type
parseVectorShapeType s = case parse' (whiteSpace >> identifierList) s of
                           Left err -> fail $ show err
                           Right names -> makeVectorShapeType $ fmap parseDimensionType names

makeVectorShapeType :: [Q Type] -> Q Type
makeVectorShapeType (d : ds) = [t| 'VectorShape $(d) $(makeTypeLevelList ds) |]
makeVectorShapeType _ = fail "Empty vectors not permitted."

parseDimensionType :: String -> Q Type
parseDimensionType n = do
                         n' <- lookupTypeName n
                         case n' of
                           Nothing -> fail ("Dimension '" ++ n ++ "' not found.")
                           Just n'' -> return $ ConT n''

makeTypeLevelList :: [Q Type] -> Q Type
makeTypeLevelList (t : ts) = [t| $(t) ': $(makeTypeLevelList ts) |]
makeTypeLevelList [] = [t| '[] |]

lexer = P.makeTokenParser haskellDef

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
commaSep1 = P.commaSep1 lexer

identifierList :: Parser [String]
identifierList = commaSep1 identifier

parse' :: Parser t -> String -> Either ParseError t
parse' p = parse p "QuasiQuote"
