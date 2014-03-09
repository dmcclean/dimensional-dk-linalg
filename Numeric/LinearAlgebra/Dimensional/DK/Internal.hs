{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.LinearAlgebra.Dimensional.DK.Internal 
{-
(
   -- * Data.Packed.Vector
   (@>),
   -- * Data.Packed.Matrix
   -- ** dimension
   cols, rows,
   colsNT, rowsNT,
   hasRows, hasCols,
   -- (><),
   trans,
   -- reshape, flatten, fromLists, toLists, buildMatrix,
   (@@>),

   -- asRow, asColumn, fromRows, toRows, fromColumns, toColumns
   -- fromBlocks
   -- diagBlock,
   -- toBlocks, toBlocksEvery, repmat, flipud, fliprl
   -- subMatrix, takeRows, dropRows, takeColumns, dropColumns,
   -- extractRows, diagRect, takeDiag, mapMatrix,
   -- mapMatrixWithIndexM, mapMatrixWithIndexM_, liftMatrix,
   -- liftMatrix2, liftMatrix2Auto, fromArray2D,

   ident, -- where to put this?
   -- * Numeric.Container
   -- constant, linspace,
   diag,
   ctrans,
   -- ** Container class
   scalar,
   conj,
   scale, scaleRecip,
   recipMat,
   add,
   sub,
   mul,
   divide,
   equal,
   arctan2,
   hconcat,
   vconcat,
   concat,
   konst,
   zeroes,
   -- build, atIndex, minIndex, maxIndex, minElement, maxElement,
   -- sumElements, prodElements, step, cond, find, assoc, accum,
   -- Convert
   -- ** Product class
   multiply,
   -- dot, absSum, norm1, norm2, normInf,
   -- norm1, normInf,
   -- optimiseMult, mXm, mXv, vXm, (<.>),
   -- (<>), (<\>), outer, kronecker,
   (<>),
   -- ** Random numbers
   -- ** Element conversion
   -- ** Input/Output
   -- ** Experimental

   -- * Numeric.LinearAlgebra.Algorithms
   -- | incomplete wrapper for "Numeric.LinearAlgebra.Algorithms"

   -- ** Linear Systems
   -- linearSolve, luSolve, cholSolve, linearSolveLS, linearSolveSVD,
   inv,
   PInv(pinv), 
   pinvTol,
   det,
   -- invlndet,
   rank,
   -- rcond,
   -- ** Matrix factorizations

   -- *** Singular value decomposition
   -- *** Eigensystems
   -- $eigs
   --wrapEig, wrapEigOnly,
   --EigCxt,
   -- **** eigenvalues and eigenvectors
   --eig,
   --eigC,
   --eigH,
   --eigH',
   --eigR,
   --eigS,
   --eigS',
   --eigSH,
   --eigSH',

   -- **** eigenvalues
   --eigOnlyC,
   --eigOnlyH,
   --eigOnlyR,
   --eigOnlyS,
   --eigenvalues,
   --eigenvaluesSH,
   --eigenvaluesSH',

   -- *** QR
   -- *** Cholesky
   -- *** Hessenberg
   -- *** Schur
   -- *** LU 

   -- ** Matrix functions
   -- sqrtm, matFunc
   expm,

   -- ** Nullspace
   -- ** Norms
   -- ** Misc
   -- ** Util 

   -- * actually internal
   toDM,
   DimMat(..),
  )
  -}
   where
import Foreign.Storable (Storable)      
import GHC.Exts (Constraint)
import Numeric.Units.Dimensional.DK.Prelude
import qualified Prelude as P
import qualified Numeric.NumType.DK as N

import Numeric.LinearAlgebra.Dimensional.DK.Shapes
import Data.Proxy

import Numeric.Matrix as M

data DimMat (shape :: Shape) a where
  DimMat :: M.Matrix a -> DimMat ('MatrixShape g rs cs) a
  DimVec :: M.Matrix a -> DimMat ('VectorShape d ds) a -- as a column vector

type ValidElement = M.MatrixElement

deriving instance (Show a, ValidElement a) => Show (DimMat s a)

{-
-- | Data.Packed.Vector.'H.@>'
(@>) :: (HNat2Integral i)
    => DimMat '[units] a
    -> Proxy i
    -> Quantity (HLookupByHNat i units) a
DimVec v @> i = Dimensional (v H.@> hNat2Integral i)

-- | Data.Packed.Matrix.'H.@@>'
(@@>) :: (HNat2Integral i, HNat2Integral j, AtEq ri i ci j ty)
    => DimMat [ri,ci] a
    -> (Proxy i, Proxy j)
    -> Quantity ty a
DimMat m @@> (i,j) = Dimensional (m H.@@> (hNat2Integral i,hNat2Integral j))

-}

{-
norm1 :: (sh ~ [r11 ': rs,ci], rs ~ MapConst r11 rs, ci ~ MapConst DOne ci, a ~ H.RealOf a)
         => DimMat sh a
         -> Quantity r11 a
norm1 (DimMat a) = Dimensional (H.pnorm H.PNorm1 a)

normInf :: (sh ~ [r11 ': rs,ci], rs ~ MapConst r11 rs, ci ~ MapConst DOne ci, a ~ H.RealOf a)
           => DimMat sh a
           -> Quantity r11 a
normInf (DimMat a) = Dimensional (H.pnorm H.Infinity a)
-}

{- | does H.'H.mXm' and H.'H.mXv'.

vXm and vXv (called dot) might be supported in the future too
-}
multiply :: (HasProduct s1 s2, ValidElement a)
    => DimMat s1 a -> DimMat s2 a
    -> DimMat (ShapeProduct s1 s2) a
multiply (DimMat a) (DimMat b) = DimMat (M.times a b)
--multiply (DimMat a) (DimVec b) = DimVec (H.mXv a b)

infixl 7 <>
(<>) :: (HasProduct s1 s2, ValidElement a)
    => DimMat s1 a -> DimMat s2 a
    -> DimMat (ShapeProduct s1 s2) a
(<>) = multiply

trans :: DimMat s a -> DimMat (ShapeTranspose s) a
trans (DimMat a) = undefined

{-
pinvTol :: (PInv sh sh',
            a ~ Double,
           sh' ~ [ri2 ': _1 , DOne ': ci2]) => Double -> DimMat sh a -> DimMat sh' a
pinvTol tol (DimMat a) = DimMat (H.pinvTol tol a)

-}

det :: (Square s) => DimMat s a -> Quantity (ShapeDeterminant s) a
det = undefined

expm :: (s ~ ShapeProduct s s, HasProduct s s) => DimMat s a -> DimMat s a
expm = undefined

scale :: Quantity d a -> DimMat s a -> DimMat (ShapeScale d s) a
scale = undefined

add :: (ValidElement a) => DimMat s a -> DimMat s a -> DimMat s a
add (DimMat x) (DimMat y) = DimMat (M.plus x y)

sub :: (ValidElement a) => DimMat s a -> DimMat s a -> DimMat s a
sub (DimMat x) (DimMat y) = DimMat (M.minus x y)

equal :: (Eq a) => DimMat s a -> DimMat s a -> Bool
equal = undefined

hconcat :: (HorizontallyConcatenable s1 s2, M.MatrixElement a) => DimMat s1 a -> DimMat s2 a -> DimMat (HorizontalConcatenation s1 s2) a
hconcat (DimMat m1) (DimMat m2) = DimMat (m1 <|> m2)
hconcat (DimMat m1) (DimVec v2) = DimMat (m1 <|> v2)
hconcat (DimVec v1) (DimMat m2) = DimMat (v1 <|> m2)
hconcat (DimVec v1) (DimVec v2) = DimMat (v1 <|> v2)

vconcat :: (VerticallyConcatenable s1 s2, M.MatrixElement a) => DimMat s1 a -> DimMat s2 a -> DimMat (VerticalConcatenation s1 s2) a
vconcat (DimMat m1) (DimMat m2) = DimMat (m1 <-> m2)
vconcat (DimMat m1) (DimVec v2) = undefined --DimMat (m1 <-> transpose v2)
vconcat (DimVec v1) (DimMat m2) = undefined --DimMat (transpose v1 <-> m2)
vconcat (DimVec v1) (DimVec v2) = DimMat (transpose v1 <-> transpose v2)

concat :: (M.MatrixElement a) => DimMat s1 a -> DimMat s2 a -> DimMat (VectorConcatenation s1 s2) a
concat (DimVec v1) (DimVec v2) = DimVec (v1 <-> v2)

rank :: DimMat s a -> Integer
rank = undefined

rows :: forall s a.(N.KnownNumType (ShapeRows s)) => DimMat s a -> Integer
rows _ = N.toNum (Proxy :: Proxy (ShapeRows s))

cols :: forall s a.(N.KnownNumType (ShapeCols s)) => DimMat s a -> Integer
cols _ = N.toNum (Proxy :: Proxy (ShapeCols s))

-- TODO: add a constraint that the row exists for better error message?
row :: forall n s a d ds.(MatrixRow s n ~ 'VectorShape d ds) => Proxy n -> DimMat s a -> DimMat ('VectorShape d ds) a
row _ _ = DimVec undefined

-- TODO: add a constraint that the column exists for better error message?
col :: forall n s a d ds.(MatrixColumn s n ~ 'VectorShape d ds) => Proxy n -> DimMat s a -> DimMat ('VectorShape d ds) a
col _ _ = DimVec undefined

{-
scalar :: (H.Field a,
          sh ~ ['[u], '[DOne]]) => Quantity u a -> DimMat sh a
scalar (Dimensional a) = DimMat (H.scalar a)
-}

{- | Numeric.Container.'H.konst', but the size is determined by the type.

>>> let n = hSucc (hSucc hZero) -- 2
>>> konst ((1::Double) *~ second) `hasRows` n `hasCols` n
2><2 1   1  
s    1.0 1.0
s    1.0 1.0

-}
{-
konst :: forall u us ones a _1.
    (H.Field a,
     HNat2Integral (HLength ones),
     HNat2Integral (HLength us),
     ones ~ (DOne ': _1),
     AllEq DOne _1,
     AllEq u us)
    => Quantity u a -> DimMat [us, ones] a
konst (Dimensional a) = DimMat (H.konst a
    (hNat2Integral (proxy :: Proxy (HLength us)),
     hNat2Integral (proxy :: Proxy (HLength ones))))

-}

-- | identity matrix. The size is determined by the type.
ident :: (HasIdentity s) => DimMat s a
ident = undefined

-- | zero matrix. The size and dimension is determined by the type.
zeroes :: forall s a.DimMat s a
zeroes = undefined

trace :: (HasTrace s) => DimMat s a -> Quantity (ShapeTrace s) a
trace = undefined

conj :: DimMat s a -> DimMat s a
conj = undefined

-- | conjugate transpose
ctrans :: DimMat s a -> DimMat (ShapeTranspose s) a
ctrans = undefined

{-

diag :: (MapConst DOne v ~ c,
        c ~ (DOne ': _1)
        ) => DimMat '[v] t -> DimMat '[v,c] t
diag (DimVec a) = DimMat (H.diag a)
-}

{- $eigs

The Hmatrix eig factors A into P and D where A = P D inv(P) and D is diagonal.

The units for eigenvalues can be figured out:

>               _____
>      -1       |  c
> P D P  = A =  |r
>               |

>       _______
>       |   d
> P   = |c
>       |

>       _______
>       |   -1
>       |  c
>  -1   |   
> P   = | -1
>       |d

So we can see that the dimension labeled `d-1` in P inverse is actually the
same `c` in `A`. The actual units of `d` don't seem to matter because the
`inv(d)` un-does any units that the `d` adds. So `d` can be all DOne. But
another choice, such as 1/c would be more appropriate, since then you can
expm your eigenvectors (not that that seems to be something people do)?

To get the row-units of A to match up, sometimes `D` will have units. 
The equation ends up as D/c = r

Please ignore the type signatures on 'eig' 'eigC' etc. instead look at the type of
'wrapEig' 'wrapEigOnly' together with the hmatrix documentation (linked).

Perhaps the convenience definitions `eig m = wrapEig H.eig m` should be in
another module.
-}

{-

-- | 'wrapEig' H.'H.eig'
eig m = wrapEig H.eig m
-- | 'wrapEig' H.'H.eigC'
eigC m = wrapEig H.eigC m
-- | 'wrapEig' H.'H.eigH'
eigH m = wrapEig H.eigH m
-- | 'wrapEig' H.'H.eigH''
eigH' m = wrapEig H.eigH' m
-- | 'wrapEig' H.'H.eigR'
eigR m = wrapEig H.eigR m
-- | 'wrapEig' H.'H.eigS'
eigS m = wrapEig H.eigS m
-- | 'wrapEig' H.'H.eigS''
eigS' m = wrapEig H.eigS' m
-- | 'wrapEig' H.'H.eigSH'
eigSH m = wrapEig H.eigSH m
-- | 'wrapEig' H.'H.eigSH''
eigSH' m = wrapEig H.eigSH' m

-- | 'wrapEigOnly' H.'H.eigOnlyC'
eigOnlyC m = wrapEigOnly H.eigOnlyC m
-- | 'wrapEigOnly' H.'H.eigOnlyH'
eigOnlyH m = wrapEigOnly H.eigOnlyH m
-- | 'wrapEigOnly' H.'H.eigOnlyR'
eigOnlyR m = wrapEigOnly H.eigOnlyR m
-- | 'wrapEigOnly' H.'H.eigOnlyS'
eigOnlyS m = wrapEigOnly H.eigOnlyS m
-- | 'wrapEigOnly' H.'H.eigenvalues'
eigenvalues m = wrapEigOnly H.eigenvalues m
-- | 'wrapEigOnly' H.'H.eigenvaluesSH'
eigenvaluesSH m = wrapEigOnly H.eigenvaluesSH m
-- | 'wrapEigOnly' H.'H.eigenvaluesSH''
eigenvaluesSH' m = wrapEigOnly H.eigenvaluesSH' m

wrapEig :: (ones ~ (DOne ': _1), EigCxt [r,c] eigVal [cinv,ones],
    H.Field y, H.Field z)
    => (H.Matrix x -> (H.Vector y, H.Matrix z)) ->
    DimMat [r,c] x ->
    (DimMat '[eigVal] y, DimMat [cinv,ones] z)
wrapEig hmatrixFun (DimMat a) = case hmatrixFun a of
    (e,v) -> (DimVec e, DimMat v)

wrapEigOnly :: (EigCxt [r,c] eigVal '(), H.Field y)
    => (H.Matrix x -> H.Vector y) ->
    DimMat [r,c] x -> DimMat '[eigVal] y
wrapEigOnly hmatrixFun (DimMat a) = case hmatrixFun a of
    (e) -> DimVec e

-}