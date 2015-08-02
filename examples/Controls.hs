{-# OPTIONS_GHC -fplugin Numeric.Units.Dimensional.DK.Solver #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Controls
where

import Numeric.Units.Dimensional.DK.Prelude
import Numeric.LinearAlgebra.Dimensional.DK.Shapes
import qualified Prelude as P

data DimMat (s :: Shape) (e :: *) = DimMat

(<>) :: (InnerProduct s1 s2 s3) => DimMat s1 e -> DimMat s2 e -> DimMat s3 e
_ <> _ = DimMat

add :: (ShapeEquivalent s1 s2) => DimMat s1 e -> DimMat s2 e -> DimMat s1 e
add _ _ = DimMat

data ContinuousLiSystem (iv :: Dimension) (xs :: [Dimension]) (ys :: [Dimension]) (us :: [Dimension]) e = ContinuousLiSystem
                                                                                                        {
                                                                                                          a'' :: DimMat (DivideVectorLists (MapDiv iv xs) xs) e,
                                                                                                          b'' :: DimMat (DivideVectorLists (MapDiv iv xs) us) e,
                                                                                                          c'' :: DimMat (DivideVectorLists ys xs) e,
                                                                                                          d'' :: DimMat (DivideVectorLists ys us) e
                                                                                                        }

type ContinuousLtiSystem = ContinuousLiSystem DTime

type ExampleSystem = ContinuousLtiSystem
    '[DLength, DVelocity, DPlaneAngle, DAngularVelocity]
    '[DLength, DPlaneAngle]
    '[DForce]
    Double

evaluate :: ContinuousLiSystem iv (x ': xs) (y ': ys) (u ': us) e -> DimMat (VectorShape x xs) e -> DimMat (VectorShape u us) e -> (DimMat (VectorShape (x / iv) (MapDiv iv xs)) e,  DimMat (VectorShape y ys) e)
evaluate sys x u = let
                      a = a'' sys
                      b = b'' sys
                      c = c'' sys
                      d = d'' sys
                      xDot = (a <> x) `add` (b <> u)
                      y = (c <> x) `add` (d <> u)
                    in (xDot, y)

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

{-
pendulum = ContinuousLiSystem {
           a'' = [matD| _0, _1, _0, _0;
                        _0, a22, a23, _0;
                        _0, _0, _0, _1;
                        _0, a42, a43, _0 |],
           b'' = [matD| _0;
                        b21;
                        _0;
                        b41 |],
           c'' = [matD| _1, _0, _0, _0;
                        _0, _0, _1, _0 |],
           d'' = zeroes
         } :: ExampleSystem
-}

pendulum = ContinuousLiSystem {
           a'' = DimMat,
           b'' = DimMat,
           c'' = DimMat,
           d'' = DimMat
         } :: ExampleSystem
