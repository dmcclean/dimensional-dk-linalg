{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Test where

import qualified Prelude as P
import Numeric.Units.Dimensional.DK.Prelude
import Numeric.LinearAlgebra.Dimensional.DK.Internal
import Numeric.LinearAlgebra.Dimensional.DK.QuasiQuotes
import Numeric.LinearAlgebra.Dimensional.DK.Shapes

frog :: [vec|  DLength  , DMass,DAmountOfSubstance, DLength/DTime |] P.Double
frog = undefined

cow :: [vec| DLength * DAmountOfSubstance |] P.Double
cow = undefined

type State = [vecShape| DPlaneAngle, DAngularVelocity, DLength, DVelocity |]

type Output = [vecShape| DPlaneAngle, DLength |]

type A = DivideVectors Output State
