{-# LANGUAGE DataKinds #-}

module Numeric.LinearAlgebra.Dimensional.DK where

import Numeric.Units.Dimensional.DK.Prelude hiding (concat)
import Numeric.LinearAlgebra.Dimensional.DK.Internal
import Numeric.LinearAlgebra.Dimensional.DK.Shapes
import Numeric.Matrix as M
import Prelude (Double)
import Data.Proxy
import qualified Numeric.NumType.DK.Nat as NN

nat0 = Proxy :: Proxy (NN.Z)
nat1 = Proxy :: Proxy (NN.S NN.Z)
nat2 = Proxy :: Proxy (NN.S (NN.S NN.Z))
nat3 = Proxy :: Proxy (NN.S (NN.S (NN.S NN.Z)))
nat4 = Proxy :: Proxy (NN.S (NN.S (NN.S (NN.S NN.Z))))

vec1 = DimVec (M.fromList [[3], [5], [7]]) :: DimMat ('VectorShape DLength '[DTime, DVelocity]) Double

hello = "world"
