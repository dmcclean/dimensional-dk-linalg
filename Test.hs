{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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

data ContinuousLiSystem (iv :: Dimension) (xs :: Shape) (ys :: Shape) (us :: Shape) v = ContinuousLiSystem
                                                                          {
                                                                            a'' :: DimMat (DivideVectors (ShapeScale (Recip iv) xs) xs) v,
                                                                            b'' :: DimMat (DivideVectors (ShapeScale (Recip iv) xs) us) v,
                                                                            c'' :: DimMat (DivideVectors ys xs) v,
                                                                            d'' :: DimMat (DivideVectors ys us) v
                                                                          }

type ContinuousLtiSystem = ContinuousLiSystem DTime


{- Example from http://ctms.engin.umich.edu/CTMS/index.php?example=InvertedPendulum&section=ControlStateSpace -}

-- not really stated on that page, but if you go back a couple of pages in their derivation
-- you can see that the type of u is a 1x1 matrix whose sole element is a force

massOfCart = (0.5 :: Double) *~ (kilo gram)
massOfPendulum = (0.2 :: Double) *~ (kilo gram)
coefficientOfFrictionForCart = (0.1 :: Double) *~ (newton / (meter / second))
lengthToPendulumCenterOfMass = (0.3 :: Double) *~ meter
massMomentOfInertiaOfPendulum = (0.006 :: Double) *~ (kilo gram * meter^pos2)
g = (9.8 :: Double) *~ (meter / second^pos2)

p = massMomentOfInertiaOfPendulum*(massOfCart+massOfPendulum)+(massOfCart*massOfPendulum*lengthToPendulumCenterOfMass*lengthToPendulumCenterOfMass)

a22 = negate (massMomentOfInertiaOfPendulum+massOfPendulum * lengthToPendulumCenterOfMass * lengthToPendulumCenterOfMass) * coefficientOfFrictionForCart / p
a23 = (massOfPendulum * massOfPendulum * g * lengthToPendulumCenterOfMass * lengthToPendulumCenterOfMass) / p
a42 = negate (massOfPendulum * lengthToPendulumCenterOfMass * coefficientOfFrictionForCart) / p
a43 = massOfPendulum * g * lengthToPendulumCenterOfMass*(massOfCart + massOfPendulum)/p

b21 = (massMomentOfInertiaOfPendulum + (massOfPendulum * lengthToPendulumCenterOfMass * lengthToPendulumCenterOfMass)) / p
b41 = massOfPendulum * lengthToPendulumCenterOfMass / p

-- example state value
x = [vec|  1.0 *~ meter,
           0.2 *~ (meter / second),
           _0 :: Dimensionless Double,
           0.1 *~ (second^neg1) |]

dx = scale (_1 / (1 *~ second)) x

-- example control input
u = [vec| (0 :: Double) *~ newton |]

-- example output
y = [vec| 1 *~ meter, _0 :: Dimensionless Double  |]

type ExampleSystem = ContinuousLtiSystem
    ('VectorShape DLength '[DVelocity, DPlaneAngle, DAngularVelocity])
    ('VectorShape DLength '[DPlaneAngle])
    ('VectorShape DForce '[])
    Double

pendulum = ContinuousLiSystem {
           a'' = zeroes,
--           a'' = [mat| _0, _1, _0, _0;
--                       _0, a22, a23, _0;
--                       _0, _0, _0, _1;
--                       _0, a42, a43, _0 |],
           --b'' = zeroes,
           b'' = [mat| _0; b21; 0 *~ (second / (kilo gram * meter)); b41 |],
           --c'' = zeroes,
           c'' = [mat| _1, _0, _0, _0;
                       _0, _0, _1, _0 |],
           d'' = zeroes
         } :: ExampleSystem

--b' = [mat| 0 *~ (second / kilo gram); b21; 0 *~ (second / (kilo gram * meter)); b41 |]
--b' = [mat| 0 *~ (second / kilo gram); b21; _0; b41 |]
b' = [mat| _0; b21; 0 *~ (second / (kilo gram * meter)); b41 |] `asTypeOf` (b'' pendulum)
