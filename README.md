dimensional-dk-linalg
=====================

Dimensionally-typed linear algebra for Haskell based on the [dimensional-dk](https://github.com/bjornbm/dimensional-dk) package.

Underlying Linear Algebra Library
---------------------------------

The initial version of dimensional-dk-linalg will be based on [hmatrix](http://hackage.haskell.org/package/hmatrix).

This may be changed in a later version because:

  * hmatrix has a more restrictive license
  * hmatrix has a dependency on the [GSL](http://www.gnu.org/software/gsl/) which makes
    it relatively unwieldy to install on Windows.

Ideally the final version will support both Windows and ARM where the author intends
to use it for simulation and robotic control purposes respectively.

Acknowledgements
----------------

  * [Björn Buckwalter](https://github.com/bjornbm/)'s dimensional and dimensional-dk packages are
    a key dependency and an inspiration.
  * Based on work by Douglas McClean and [Adam Vogt](https://github.com/aavogt).
    This package differs from [DimMat](https://github.com/aavogt/DimMat) in that the intent is primarily to check dimensions,
    and not so much to infer them in a "backwards" direction (from result types
    to argument types).