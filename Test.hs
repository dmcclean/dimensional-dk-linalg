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

frog :: [vec|  DLength  , DMass,DLength/DTime |] P.Double
frog = [vec| 3 *~ meter, 41.2 *~ kilo gram, 11.2 *~ knot |]

cow :: [vec| DLength * DAmountOfSubstance |] P.Double
cow = undefined

type State = [vecShape| DPlaneAngle, DAngularVelocity, DLength, DVelocity |]

type Output = [vecShape| DPlaneAngle, DLength |]

i = undefined :: DimMat State Double
o = undefined :: DimMat Output Double

type A = DivideVectors Output State

a :: DimMat A Double
a = [mat| _1, _0, _0, _0;
          _0, _0, _1, _0 |]

b = [mat| (1.0 :: Double) *~ meter, 2.0 *~ (meter / second);
          7.3 *~ (meter/second), -1.4 *~ (meter / second^pos2) |]

moose = "squirrel"
