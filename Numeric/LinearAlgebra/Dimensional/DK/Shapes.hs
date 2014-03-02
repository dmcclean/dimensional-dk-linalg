{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}
{- | This module only exposes the types and type functions neccessary to express linear algebra, it doesn't actually implement term-level linear algebra.
-}
module Numeric.LinearAlgebra.Dimensional.DK.Shapes (
  MatrixShape(..),
  VectorShape,
  HasProduct,
  ShapeScale,
  ShapeProduct,
  ShapeTranspose,
  ShapeInverse,
  ShapeDeterminant,
  ShapeDimensionless,
  ShapeRows,
  ShapeCols,
  HasIdentity,
  HasTrace,
  ShapeTrace,
  DivideVectors,
  VectorLength,
  Square,
  HorizontallyConcatenable,
  VerticallyConcatenable,
  HorizontalConcatenation,
  VerticalConcatenation,
  MatrixElement,
  VectorElement,
  MatrixRow,
  MatrixColumn,
  ) where

import GHC.Exts (Constraint)
import Numeric.Units.Dimensional.DK.Prelude
import qualified Prelude as P
import qualified Numeric.NumType.DK as N
import qualified Numeric.NumType.DK.Nat as NN

-- define a data kind for matrix shapes
-- a matrix shape is a single global dimension, an n-1 list of row dimesnions, and an m-1 list of column dimensions
data MatrixShape = MatrixShape Dimension [Dimension] [Dimension]

-- define a data kind for vector shapes
data VectorShape = VectorShape Dimension [Dimension]


type family ShapeScale (d :: Dimension) (s :: MatrixShape) :: MatrixShape where
  ShapeScale d ('MatrixShape g rs cs) = 'MatrixShape (d * g) rs cs


-- Define the circumstances under which matrices have a product.
type family HasProduct (ldims :: MatrixShape) (rdims :: MatrixShape) :: Constraint where
  HasProduct ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = (cs1 ~ MapInv rs2)


-- Define the shape of matrix products.
-- This is defined even where the product doesn't exist (non-matching dimensions or sizes), but no problem arises because you can't call the actual term-level product method at such shapes.
type family ShapeProduct (ldims :: MatrixShape) (rdims :: MatrixShape) :: MatrixShape where
  ShapeProduct ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = 'MatrixShape (g1 * g2) rs1 cs2


-- Define the shape of matrix transposition.
-- The shape of matrix transposition for shapes expressed in this form is simple, just flip the rows and columns.
type family ShapeTranspose (mdims :: MatrixShape) :: MatrixShape where
  ShapeTranspose ('MatrixShape g rs cs) = 'MatrixShape g cs rs


-- Define the shape of matrix inversion.
type family ShapeInverse (mdims :: MatrixShape) :: MatrixShape where
  ShapeInverse ('MatrixShape g rs cs) = 'MatrixShape (Inverse g) (MapInv cs) (MapInv rs)


-- Define the type of a matrix determinant.
-- This is defined even where the determinant doesn't exist (non-square shapes), but no problem arises because you can't call the actual term-level determinant method at non-square shapes.
type family ShapeDeterminant (shape :: MatrixShape) :: Dimension where
  ShapeDeterminant ('MatrixShape g rs cs) = g * ((DimProduct rs) * (DimProduct cs))


-- Define the type of a matrix the same size, but with dimensions stripped.
type family ShapeDimensionless (shape :: MatrixShape) :: MatrixShape where
  ShapeDimensionless ('MatrixShape g rs cs) = 'MatrixShape DOne (MapConstOne cs) (MapConstOne cs)


-- Constrain matrix shapes that have an identity matrix.
type family HasIdentity (shape :: MatrixShape) :: Constraint where
  HasIdentity ('MatrixShape g rs cs) = (g ~ DOne, rs ~ MapInv cs)


type family HasTrace (shape :: MatrixShape) :: Constraint where
  HasTrace ('MatrixShape g rs cs) = (rs ~ MapInv cs)

type family ShapeTrace (shape :: MatrixShape) :: Dimension where
  ShapeTrace ('MatrixShape g rs cs) = g


-- Define the type-level number of rows in a matrix.
type family ShapeRows (shape :: MatrixShape) :: N.NumType where
  ShapeRows ('MatrixShape g rs cs) = N.Pos1 N.+ (ListLength rs)


-- Define the type-level number of columns in a matrix.
type family ShapeCols (shape :: MatrixShape) :: N.NumType where
  ShapeCols ('MatrixShape g rs cs) = N.Pos1 N.+ (ListLength cs)


type family VectorLength (shape :: VectorShape) :: N.NumType where 
  VectorLength ('VectorShape a as) = N.Pos1 N.+ (ListLength as)


-- A constraint for square matrices.
type family Square (shape :: MatrixShape) :: Constraint where
  Square ('MatrixShape g rs cs) = (ListLength rs ~ ListLength cs)


-- A matrix shape for converting from one vector to another.
-- This is the shape that, when right-multiplied by a column vector whose shape is from, produces a column vector whose shape is to.
type family DivideVectors (to :: VectorShape) (from :: VectorShape) :: MatrixShape where
  DivideVectors ('VectorShape t ts) ('VectorShape f fs) = 'MatrixShape 
                                                             (ListHead (MapDiv (ListHead (f ': fs)) (t ': ts)))
                                                             (MapDiv (ListHead (f ': fs)) (t ': ts))
                                                             (MapMul (ListHead (f ': fs)) (MapInv (f ': fs)))



type family HorizontallyConcatenable (s1 :: MatrixShape) (s2 :: MatrixShape) :: Constraint where
  HorizontallyConcatenable ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = (g1 ~ g2, rs1 ~ rs2)

type family HorizontalConcatenation (s1 :: MatrixShape) (s2 :: MatrixShape) :: MatrixShape where
  HorizontalConcatenation ('MatrixShape g rs1 cs) ('MatrixShape g rs2 cs) = 'MatrixShape g (ListAppend rs1 rs2) cs

type family VerticallyConcatenable (s1 :: MatrixShape) (s2 :: MatrixShape) :: Constraint where
  VerticallyConcatenable ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = (g1 ~ g2, cs1 ~ cs2)

type family VerticalConcatenation (s1 :: MatrixShape) (s2 :: MatrixShape) :: MatrixShape where
  VerticalConcatenation ('MatrixShape g rs cs1) ('MatrixShape g rs cs2) = 'MatrixShape g rs (ListAppend cs1 cs2)



-- Extract the dimension of an element from a shape.
type family MatrixElement (shape :: MatrixShape) (row :: NN.Nat) (col :: NN.Nat) :: Dimension where
  MatrixElement ('MatrixShape g rs cs) i j = g * ((ElementAt (DOne ': rs) i) * (ElementAt (DOne ': cs) j))


-- Extract the dimension of an element from a vector shape.
type family VectorElement (shape :: VectorShape) (i :: NN.Nat) :: Dimension where
  VectorElement ('VectorShape d ds) NN.Z = d
  VectorElement ('VectorShape d ds) (NN.S i) = ElementAt ds i

-- Extract a row from a matrix.
type family MatrixRow (shape :: MatrixShape) (row :: NN.Nat) :: VectorShape where
  MatrixRow ('MatrixShape g rs cs) row = 'VectorShape (g * ElementAt (DOne ': rs) row) (MapMul (g * ElementAt (DOne ': rs) row) cs)

-- Extract a column from a matrix.
type family MatrixColumn (shape :: MatrixShape) (col :: NN.Nat) :: VectorShape where
  MatrixColumn ('MatrixShape g rs cs) col = 'VectorShape (g * ElementAt (DOne ': cs) col) (MapMul (g * ElementAt (DOne ': cs) col) rs)

-- Invert all dimensions in a list of dimensions.
type family MapInv (dims :: [Dimension]) :: [Dimension] where
  MapInv '[] = '[]
  MapInv (x ': xs) = (Inverse x) ': (MapInv xs)


-- Convert all dimensions in a list of dimensions to dimensionless.
type family MapConstOne (dims :: [Dimension]) :: [Dimension] where
  MapConstOne '[] = '[]
  MapConstOne (x ': xs) = DOne ': (MapConstOne xs)


-- Multiply a list of dimensions by a dimension.
type family MapMul (dim :: Dimension) (dims :: [Dimension]) :: [Dimension] where
  MapMul d '[] = '[]
  MapMul d (x ': xs) = (d * x ': (MapMul d xs))


-- Divide a list of dimensions by a dimension.
type family MapDiv (dim :: Dimension) (dims :: [Dimension]) :: [Dimension] where
  MapDiv d '[] = '[]
  MapDiv d (x ': xs) = (x / d ': (MapDiv d xs))


-- Define the product of a list of dimensions.
type family DimProduct (dims :: [Dimension]) :: Dimension where
  DimProduct '[] = DOne
  DimProduct (a ': as) = a * (DimProduct as)


-- Get the head of a type-level list.
type family ListHead (xs :: [k]) :: k where
  ListHead (x ': xs) = x


-- Get the length of a type-level list.
type family ListLength (xs :: [k]) :: N.NumType where
  ListLength '[] = N.Zero
  ListLength (x ': xs) = N.Pos1 N.+ (ListLength xs)


type family ListAppend (xs :: [k]) (ys :: [k]) :: [k] where
  ListAppend '[] ys = ys
  ListAppend (x ': xs) ys = x ': (ListAppend xs ys)



-- Get a specified, zero-indexed element from a type-level list.
type family ElementAt (xs :: [k]) (n :: NN.Nat) :: k where
  ElementAt (a ': as) NN.Z = a
  ElementAt (a ': as) (NN.S n) = ElementAt as n
