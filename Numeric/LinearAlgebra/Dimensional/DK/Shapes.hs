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
  Shape(..),
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
  VectorConcatenation,
  MatrixElement,
  VectorElement,
  MatrixRow,
  MatrixColumn,
  MapDiv,
  MapMul,
  ) where

import GHC.Exts (Constraint)
import Numeric.Units.Dimensional.DK.Prelude
import qualified Prelude as P
import qualified Numeric.NumType.DK as N
import qualified Numeric.NumType.DK.Nat as NN

-- define a data kind for matrix shapes
-- a matrix shape is a single global dimension, an n-1 list of row dimesnions, and an m-1 list of column dimensions
data Shape = MatrixShape Dimension [Dimension] [Dimension]
           | VectorShape Dimension [Dimension]


type family ShapeScale (d :: Dimension) (s :: Shape) :: Shape where
  ShapeScale d ('MatrixShape g rs cs) = 'MatrixShape (d * g) rs cs
  ShapeScale d ('VectorShape x xs) = 'VectorShape (d * x) (MapMul d xs)


-- Define the circumstances under which an inner product exists.
type family HasProduct (ldims :: Shape) (rdims :: Shape) :: Constraint where
  HasProduct ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = (cs1 ~ MapRecip rs2)
  -- other entries for matrix/vector products

-- Define the shape of inner products.
-- This is defined even where the product doesn't exist (non-matching dimensions or sizes), but no problem arises because you can't call the actual term-level product method at such shapes.
type family ShapeProduct (ldims :: Shape) (rdims :: Shape) :: Shape where
  ShapeProduct ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = 'MatrixShape (g1 * g2) rs1 cs2
  -- other entries for matrix/vector products

-- Define the shape of transposition.
-- The shape of matrix transposition for shapes expressed in this form is simple, just flip the rows and columns.
type family ShapeTranspose (mdims :: Shape) :: Shape where
  ShapeTranspose ('MatrixShape g rs cs) = 'MatrixShape g cs rs


-- Define the shape of matrix inversion.
type family ShapeInverse (mdims :: Shape) :: Shape where
  ShapeInverse ('MatrixShape g rs cs) = 'MatrixShape (Recip g) (MapRecip cs) (MapRecip rs)


-- Define the type of a matrix determinant.
-- This is defined even where the determinant doesn't exist (non-square shapes), but no problem arises because you can't call the actual term-level determinant method at non-square shapes.
type family ShapeDeterminant (shape :: Shape) :: Dimension where
  ShapeDeterminant ('MatrixShape g rs cs) = g * ((DimProduct rs) * (DimProduct cs))


-- Define the type of a matrix the same size, but with dimensions stripped.
type family ShapeDimensionless (shape :: Shape) :: Shape where
  ShapeDimensionless ('MatrixShape g rs cs) = 'MatrixShape DOne (MapConstOne cs) (MapConstOne cs)
  ShapeDimensionless ('VectorShape d ds) = 'VectorShape DOne (MapConstOne ds)


-- Constrain matrix shapes that have an identity matrix.
type family HasIdentity (shape :: Shape) :: Constraint where
  HasIdentity ('MatrixShape g rs cs) = (g ~ DOne, rs ~ MapRecip cs)


type family HasTrace (shape :: Shape) :: Constraint where
  HasTrace ('MatrixShape g rs cs) = (rs ~ MapRecip cs)

type family ShapeTrace (shape :: Shape) :: Dimension where
  ShapeTrace ('MatrixShape g rs cs) = g


-- Define the type-level number of rows in a matrix.
type family ShapeRows (shape :: Shape) :: NN.Nat where
  ShapeRows ('MatrixShape g rs cs) = NN.S (ListLength rs)


-- Define the type-level number of columns in a matrix.
type family ShapeCols (shape :: Shape) :: NN.Nat where
  ShapeCols ('MatrixShape g rs cs) = NN.S (ListLength cs)


type family VectorLength (shape :: Shape) :: NN.Nat where 
  VectorLength ('VectorShape a as) = NN.S (ListLength as)


-- A constraint for square matrices.
type family Square (shape :: Shape) :: Constraint where
  Square ('MatrixShape g rs cs) = (ListLength rs ~ ListLength cs)


-- A matrix shape for converting from one vector to another.
-- This is the shape that, when right-multiplied by a column vector whose shape is from, produces a column vector whose shape is to.
type family DivideVectors (to :: Shape) (from :: Shape) :: Shape where
  DivideVectors ('VectorShape t ts) ('VectorShape f fs) = 'MatrixShape 
                                                             (ListHead (MapDiv (ListHead (f ': fs)) (t ': ts)))
                                                             (MapDiv (ListHead (f ': fs)) ts)
                                                             (MapMul (ListHead (f ': fs)) (MapRecip fs))
  -- try to deal with matrix/matrix division?


type family HorizontallyConcatenable (s1 :: Shape) (s2 :: Shape) :: Constraint where
  HorizontallyConcatenable ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = (rs1 ~ rs2)
  HorizontallyConcatenable ('MatrixShape g1 rs1 cs1) ('VectorShape r2 rs2)     = (g1 ~ r2, rs2 ~ MapMul g1 rs1)
  HorizontallyConcatenable ('VectorShape r1 rs1)     ('MatrixShape g2 rs2 cs2) = (g2 ~ r1, rs1 ~ MapMul g2 rs2)
  HorizontallyConcatenable ('VectorShape r1 rs1)     ('VectorShape r2 rs2)     = (rs1 ~ MapMul (r1 / r2) rs2)

type family HorizontalConcatenation (s1 :: Shape) (s2 :: Shape) :: Shape where
  HorizontalConcatenation ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = 'MatrixShape g1 (ListAppend rs1 (MapMul (g2 / g1) rs2)) cs1
  HorizontalConcatenation ('MatrixShape g1 rs1 cs1) ('VectorShape r2 rs2)     = 'MatrixShape g1 rs1 (ListAppend cs1 '[r2 / g1])
  HorizontalConcatenation ('VectorShape r1 rs1)     ('MatrixShape g2 rs2 cs2) = 'MatrixShape r1 (MapDiv r1 rs1) (MapDiv r1 (g2 ': (MapMul g2 cs2)))
  HorizontalConcatenation ('VectorShape r1 rs1)     ('VectorShape r2 rs2)     = 'MatrixShape r1 (MapDiv r1 rs1) '[r2 / r1]

type family VerticallyConcatenable (s1 :: Shape) (s2 :: Shape) :: Constraint where
  VerticallyConcatenable ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2) = (cs1 ~ cs2)
  VerticallyConcatenable ('MatrixShape g1 rs1 cs1) ('VectorShape c2 cs2)     = (g1 ~ c2, cs2 ~ MapMul g1 cs1)
  VerticallyConcatenable ('VectorShape c1 cs1)     ('MatrixShape g2 rs2 cs2) = (g2 ~ c1, cs1 ~ MapMul g2 cs2)
  VerticallyConcatenable ('VectorShape c1 cs1)     ('VectorShape c2 cs2)     = (cs1 ~ MapMul (c1 / c2) cs2)

type family VerticalConcatenation (s1 :: Shape) (s2 :: Shape) :: Shape where
  VerticalConcatenation ('MatrixShape g1 rs1 cs1) ('MatrixShape g2 rs2 cs2)  = 'MatrixShape g1 rs1 (ListAppend cs1 (MapMul (g2 / g1) cs2))
  -- VerticalConcatenation ('MatrixShape g1 rs1 cs1) ('VectorShape c2 cs2)     = 'MatrixShape ...
  -- VerticalConcatenation ('VectorShape c1 cs1)     ('MatrixShape g2 rs2 cs2) = 'MatrixShape ...
  VerticalConcatenation ('VectorShape c1 cs1)   ('VectorShape c2 cs2)   = 'MatrixShape c1 '[c2 / c1] (MapDiv c1 cs1)

type family VectorConcatenation (s1 :: Shape) (s2 :: Shape) :: Shape where
  VectorConcatenation ('VectorShape d1 ds1) ('VectorShape d2 ds2) = 'VectorShape d1 (ListAppend ds1 (d2 ': ds2))

-- Extract the dimension of an element from a shape.
type family MatrixElement (shape :: Shape) (row :: NN.Nat) (col :: NN.Nat) :: Dimension where
  MatrixElement ('MatrixShape g rs cs) i j = g * ((ElementAt (DOne ': rs) i) * (ElementAt (DOne ': cs) j))


-- Extract the dimension of an element from a vector shape.
type family VectorElement (shape :: Shape) (i :: NN.Nat) :: Dimension where
  VectorElement ('VectorShape d ds) NN.Z = d
  VectorElement ('VectorShape d ds) (NN.S i) = ElementAt ds i

-- Extract a row from a matrix.
type family MatrixRow (matrix :: Shape) (row :: NN.Nat) :: Shape where
  MatrixRow ('MatrixShape g rs cs) row = 'VectorShape (g * ElementAt (DOne ': rs) row) (MapMul (g * ElementAt (DOne ': rs) row) cs)

-- Extract a column from a matrix.
type family MatrixColumn (matrix :: Shape) (col :: NN.Nat) :: Shape where
  MatrixColumn ('MatrixShape g rs cs) col = 'VectorShape (g * ElementAt (DOne ': cs) col) (MapMul (g * ElementAt (DOne ': cs) col) rs)

-- Invert all dimensions in a list of dimensions.
type family MapRecip (dims :: [Dimension]) :: [Dimension] where
  MapRecip '[] = '[]
  MapRecip (x ': xs) = (Recip x) ': (MapRecip xs)


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
type family ListLength (xs :: [k]) :: NN.Nat where
  ListLength '[] = NN.Z
  ListLength (x ': xs) = NN.S (ListLength xs)


type family ListAppend (xs :: [k]) (ys :: [k]) :: [k] where
  ListAppend '[] ys = ys
  ListAppend (x ': xs) ys = x ': (ListAppend xs ys)



-- Get a specified, zero-indexed element from a type-level list.
type family ElementAt (xs :: [k]) (n :: NN.Nat) :: k where
  ElementAt (a ': as) NN.Z = a
  ElementAt (a ': as) (NN.S n) = ElementAt as n
