{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Test where

import qualified Prelude as P
import Numeric.Units.Dimensional.DK.Prelude
import Numeric.LinearAlgebra.Dimensional.DK.Internal
import Numeric.LinearAlgebra.Dimensional.DK.QuasiQuotes

frog :: [vecD|  DLength  , DMass,DAmountOfSubstance |] P.Double
frog = undefined
