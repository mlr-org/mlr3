# Install (Missing) Packages

`extract_pkgs()` extracts required package from various objects,
including
[TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.md),
[Learner](https://mlr3.mlr-org.com/reference/Learner.md),
[Measure](https://mlr3.mlr-org.com/reference/Measure.md) and objects
from extension packages such as
[mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines) or
[mlr3filters](https://CRAN.R-project.org/package=mlr3filters). If
applied on a list, the function is called recursively on all elements.

`install_pkgs()` calls `extract_pkgs()` internally and proceeds with the
installation of extracted packages.

## Usage

``` r
install_pkgs(x, ...)

extract_pkgs(x)

# S3 method for class 'character'
extract_pkgs(x)

# S3 method for class 'R6'
extract_pkgs(x)

# S3 method for class 'list'
extract_pkgs(x)

# S3 method for class 'ResampleResult'
extract_pkgs(x)

# S3 method for class 'BenchmarkResult'
extract_pkgs(x)
```

## Arguments

- x:

  (any)  
  Object with package information (or a list of such objects).

- ...:

  (any)  
  Additional arguments passed down to
  [`remotes::install_cran()`](https://remotes.r-lib.org/reference/install_cran.html)
  or
  [`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).
  Arguments `force` and `upgrade` are often important in this context.

## Value

`extract_pkgs()` returns a
[`character()`](https://rdrr.io/r/base/character.html) of package
strings, `install_pkgs()` returns the names of extracted packages
invisibly.

## Details

If a package contains a forward slash ('/'), it is assumed to be a
package hosted on GitHub in `"<user>/<repo>"` format, and the string
will be passed to
[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).
Otherwise, the package name will be passed to
[`remotes::install_cran()`](https://remotes.r-lib.org/reference/install_cran.html).

## Examples

``` r
extract_pkgs(lrns(c("regr.rpart", "regr.featureless")))
#> [1] "mlr3"  "rpart" "stats"
```
