# kantorovich 3.1.0

* New solving method using the 'ompr' package. This method is slower but 
its implementation has a very nice syntax.


# kantorovich 3.0.1

* Changes required for the new version of 'gmp'.


# kantorovich 3.0.0

* New function `kantorovich_CVX`; it beats `kantorovich_glpk` and `kantorovich_lp`.


# kantorovich 2.0.1

* Fixed some errors raised by the CRAN checks.


# kantorovich 2.0.0

* New functions `kantorovich_glpk` and `kantorovich_lp`.


# kantorovich 1.1.1

* Import 'gmp' in NAMESPACE.


# kantorovich 1.1.0

* Now the package accepts `mu` and `nu` as character vectors (then treated as `bigq` vectors).

* `kantorovich()` with option `details=TRUE` returns details of the Kantorovich computation.


# kantorovich 1.0.0

First release.


