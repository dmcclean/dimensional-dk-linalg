{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
-- | a variation on <http://hackage.haskell.org/package/hmatrix-syntax>,
-- which constructs a matrix which has dimensions stored.
module Numeric.LinearAlgebra.Dimensional.DK.QuasiQuotes
(
  vec,
  vecShape,
  mat,
) where

import Language.Haskell.TH

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse as Meta

import Numeric.Units.Dimensional.DK (Quantity, Dimension, DLength, DMass)
import Numeric.LinearAlgebra.Dimensional.DK.Internal (DimMat(..), vecSingleton, vecCons, fromRowVector, vconcat, vconcat', vconcat'')
import Numeric.LinearAlgebra.Dimensional.DK.Shapes

import Data.List.Split


vec = QuasiQuoter {
  quoteExp = parseVectorExp,
  quotePat = error "vec",
  quoteDec = error "vec",
  quoteType = parseVectorType
}

vecShape = QuasiQuoter {
  quoteExp = error "vecShape",
  quotePat = error "vecShape",
  quoteDec = error "vecShape",
  quoteType = parseVectorShapeType
}

mat = QuasiQuoter {
  quoteExp = parseMatrixExp,
  quotePat = error "mat",
  quoteDec = error "mat",
  quoteType = error "mat"
}

parseMatrixExp :: String -> Q Exp
parseMatrixExp s = let rs = fmap parseVectorExp $ splitSemicolonList s
                    in makeMatrixExp rs

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

makeMatrixExp :: [Q Exp] -> Q Exp
makeMatrixExp [] = fail "Empty matrices not permitted."
makeMatrixExp [e] = [| fromRowVector $(e) |]
makeMatrixExp (e:[e2]) = [| vconcat'' $(e) $(e2) |]
makeMatrixExp (e:es) = [| vconcat' $(e) $(makeMatrixExp es) |]

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
