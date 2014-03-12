{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
-- | a variation on <http://hackage.haskell.org/package/hmatrix-syntax>,
-- which constructs a matrix which has dimensions stored.
module Numeric.LinearAlgebra.Dimensional.DK.QuasiQuotes
(
  vec,
  vecShape,
) where

import Language.Haskell.TH

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse as Meta

import Numeric.Units.Dimensional.DK (Quantity, Dimension, DLength, DMass)
import Numeric.LinearAlgebra.Dimensional.DK.Internal (DimMat(..), vecSingleton, vecCons)
import Numeric.LinearAlgebra.Dimensional.DK.Shapes

import Data.List.Split


vec = QuasiQuoter {
  quoteExp = parseVectorExp,
  quotePat = error "vecD",
  quoteDec = error "vecD",
  quoteType = parseVectorType
}

vecShape = QuasiQuoter {
  quoteExp = error "vecShapeD",
  quotePat = error "vecShapeD",
  quoteDec = error "vecShapeD",
  quoteType = parseVectorShapeType
}

parseVectorExp :: String -> Q Exp
parseVectorExp s = let qs = fmap parseQuantityExp $ splitCommaList s
                    in makeVectorExp qs

parseQuantityExp :: String -> Q Exp
parseQuantityExp s = do
                       case (Meta.parseExp s) of
                         Left err -> fail $ show err
                         Right e -> return e

parseVectorType :: String -> Q Type
parseVectorType s = [t| DimMat $(parseVectorShapeType s) |]

parseVectorShapeType :: String -> Q Type
parseVectorShapeType s = let dimTypes = fmap parseDimensionType $ splitCommaList s
                          in makeVectorShapeType dimTypes

parseDimensionType :: String -> Q Type
parseDimensionType s = do
                          case (Meta.parseType s) of
                            Left err -> fail $ show err
                            Right t -> return t

makeVectorExp :: [Q Exp] -> Q Exp
makeVectorExp [] = fail "Empty vectors not permitted."
makeVectorExp [e] = [| vecSingleton $(e) |]
makeVectorExp (e:es) = [| vecCons $(e) $(makeVectorExp es) |]

makeVectorShapeType :: [Q Type] -> Q Type
makeVectorShapeType (d : ds) = [t| 'VectorShape $(d) $(makeTypeLevelList ds) |]
makeVectorShapeType _ = fail "Empty vectors not permitted."

makeTypeLevelList :: [Q Type] -> Q Type
makeTypeLevelList (t : ts) = [t| $(t) ': $(makeTypeLevelList ts) |]
makeTypeLevelList [] = [t| '[] |]

splitCommaList :: String -> [String]
splitCommaList = splitOn ","

splitSemicolonList :: String -> [String]
splitSemicolonList = splitOn ";"
