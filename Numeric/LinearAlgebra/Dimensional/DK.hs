{-# LANGUAGE DataKinds #-}

module Numeric.LinearAlgebra.Dimensional.DK where

import Numeric.Units.Dimensional.DK.Prelude hiding (concat)
import Numeric.LinearAlgebra.Dimensional.DK.Internal
import Numeric.LinearAlgebra.Dimensional.DK.Shapes
import Numeric.Matrix as M
import Prelude (Double)

vec1 = DimVec (M.fromList [[3], [5], [7]]) :: DimMat ('VectorShape DLength '[DTime, DVelocity]) Double

hello = "world"
