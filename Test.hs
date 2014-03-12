{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NegativeLiterals #-}

module Test where

import qualified Prelude as P
import Numeric.Units.Dimensional.DK.Prelude
import Numeric.Units.Dimensional.DK.NonSI
import Numeric.LinearAlgebra.Dimensional.DK.Internal
import Numeric.LinearAlgebra.Dimensional.DK.QuasiQuotes
import Numeric.LinearAlgebra.Dimensional.DK.Shapes

-- for selecting elements in ghci
import qualified Numeric.NumType.DK.Nat as NN
import Data.Proxy

nat0 = Proxy :: Proxy (NN.Z)
nat1 = Proxy :: Proxy (NN.S NN.Z)
nat2 = Proxy :: Proxy (NN.S (NN.S NN.Z))
nat3 = Proxy :: Proxy (NN.S (NN.S (NN.S NN.Z)))
nat4 = Proxy :: Proxy (NN.S (NN.S (NN.S (NN.S NN.Z))))
-- end of ghci helpers

frog :: [vec|  DLength  , DMass,DLength/DTime |] P.Double
frog = [vec| 3 *~ meter, 41.2 *~ kilo gram, 11.2 *~ knot |]

cow :: [vec| DLength * DAmountOfSubstance |] P.Double
cow = undefined

type State = [vecShape| DPlaneAngle, DAngularVelocity, DLength, DVelocity |]

type Output = [vecShape| DPlaneAngle, DLength |]

i = undefined :: DimMat State Double
o = undefined :: DimMat Output Double

type A = DivideVectors Output State

type A' = MatrixShape DPlaneAngle '[DLength] '[DTime, DWaveNumber, Recip DVelocity]

a :: DimMat A' Double
--a = zeroes
a = [mat| _1, _0, _0, _0;
          _0, _0, _1, _0 |]
--a = [mat| _1, 0 *~ second, 0 *~ (meter^neg1), 0 *~ (second/meter); 0 *~ meter, 0 *~ (second * meter), _1, 0 *~ second |]
{-
a = vconcat (vecCons _1 
              (vecCons (0 *~ second)
                (vecCons (0 *~ (meter^neg1))
                  (vecSingleton (0 *~ (second/meter))))))
            (vecCons (0 *~ meter)
              (vecCons (0 *~ (second * meter))
                (vecCons _1
                  (vecSingleton (0 *~ second)))))
-}
--a = vconcat' (vecCons _1 (vecCons _0 (vecCons _0 (vecSingleton _0)))) (vecCons _0 (vecCons _0 (vecCons _1 (vecSingleton _0))))
--a = vconcat undefined undefined

b = [mat| (1.0 :: Double) *~ meter, 2.0 *~ (meter / second);
          7.3 *~ (meter/second), -1.4 *~ (meter / second^pos2) |]

c = [mat| _1; _1; (2.3 :: Double) *~ mole |]

d = [mat| (2.7 :: Double) *~ mile |]

e = [mat| _2 :: Dimensionless Double, _3 |]

f :: DimMat State Double
f = [vec| _0, _0, _0, _0 |]

moose = "squirrel"
