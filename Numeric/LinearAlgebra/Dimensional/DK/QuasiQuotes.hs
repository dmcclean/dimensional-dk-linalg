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
import Numeric.LinearAlgebra.Dimensional.DK.Internal (DimMat(..))
import Numeric.LinearAlgebra.Dimensional.DK.Shapes

import Data.List.Split


vec = QuasiQuoter {
  quoteExp = error "vecD",
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

parseVectorType :: String -> Q Type
parseVectorType s = [t| DimMat $(parseVectorShapeType s) |]
--parseVectorType s = [t| DimMat ('VectorShape $(parseDimensionType' s) '[]) |]

parseVectorShapeType :: String -> Q Type
parseVectorShapeType s = let dimTypes = fmap parseDimensionType $ splitOn "," s
                          in makeVectorShapeType dimTypes

makeVectorShapeType :: [Q Type] -> Q Type
makeVectorShapeType (d : ds) = [t| 'VectorShape $(d) $(makeTypeLevelList ds) |]
makeVectorShapeType _ = fail "Empty vectors not permitted."

parseDimensionType :: String -> Q Type
parseDimensionType s = do
                          case (Meta.parseType s) of
                            Left err -> fail $ show err
                            Right t -> return t

makeTypeLevelList :: [Q Type] -> Q Type
makeTypeLevelList (t : ts) = [t| $(t) ': $(makeTypeLevelList ts) |]
makeTypeLevelList [] = [t| '[] |]
