{-# LANGUAGE DataKinds #-}

module Numeric.LinearAlgebra.Dimensional.DK where

import Numeric.Units.Dimensional.DK.Prelude hiding (concat)
import Numeric.LinearAlgebra.Dimensional.DK.Internal
import Numeric.LinearAlgebra.Dimensional.DK.Shapes
import Prelude (Double)

vec1 = DimVec (undefined) :: DimMat ('VectorShape DLength '[DTime, DVelocity]) Double

hello = "world"
