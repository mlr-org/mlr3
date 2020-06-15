
# mlr3 <img src="man/figures/logo.png" align="right" width = "120" />

Package website: [release](https://mlr3.mlr-org.com/) |
[dev](https://mlr3.mlr-org.com/dev)

Efficient, object-oriented programming on the building blocks of machine
learning. Successor of [mlr](https://github.com/mlr-org/mlr).

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3/actions)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.01903/status.svg)](https://doi.org/10.21105/joss.01903)
[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3)](https://cran.r-project.org/package=mlr3)
[![Cran
Checks](https://cranchecks.info/badges/worst/mlr3)](https://cran.r-project.org/web/checks/check_results_mlr3.html)
[![codecov](https://codecov.io/gh/mlr-org/mlr3/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Dependencies](https://tinyverse.netlify.com/badge/mlr3)](https://cran.r-project.org/package=mlr3)
<!-- badges: end -->

## Resources (for users and developers)

  - We *started* writing a [book](https://mlr3book.mlr-org.com/). This
    should be the central entry point to the package.
  - The [mlr3gallery](https://mlr3gallery.mlr-org.com) has some case
    studies and demonstrates how frequently occurring problems can be
    solved. It is still in early days so stay tuned for more to come.
  - **Videos**: Currently 2 recorded talks exist from useR\!2019. Both
    are somewhat outdated already but can be useful to get a first
    overview about the package ecosystem and design ideas.
      - [mlr3](https://www.youtube.com/watch?v=wsP2hiFnDQs)
      - [mlr3pipelines and
        mlr3tuning](https://www.youtube.com/watch?v=gEW5RxkbQuQ)
  - [mlr3 reference manual](https://mlr3.mlr-org.com/reference/)
  - **Cheatsheets**
      - [Overview of cheatsheets](https://cheatsheets.mlr-org.com)
      - [mlr3](https://cheatsheets.mlr-org.com/mlr3.pdf)
      - [mlr3tuning](https://cheatsheets.mlr-org.com/mlr3tuning.pdf)
      - [mlr3pipelines](https://cheatsheets.mlr-org.com/mlr3pipelines.pdf)
  - **Templates/Tutorials**
      - [mlr3-learndrake](https://github.com/mlr-org/mlr3-learndrake):
        Shows how to use mlr3 with
        [drake](https://docs.ropensci.org/drake/) for reproducible ML
        workflow automation.
  - [List of extension
    packages](https://github.com/mlr-org/mlr3/wiki/Extension-Packages)
  - [mlr-outreach](https://github.com/mlr-org/mlr-outreach) contains
    public talks and slides resources.
  - Our [blog](https://mlr-org.com/) about *mlr* and *mlr3*. (We are not
    the most frequent bloggers ;) )
  - [Wiki](https://github.com/mlr-org/mlr3/wiki): Contains mainly
    information for developers.

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3")
```

## Example

### Constructing Learners and Tasks

``` r
library(mlr3)

# create learning task
task_iris <- TaskClassif$new(id = "iris", backend = iris, target = "Species")
task_iris
```

    ## <TaskClassif:iris> (150 x 5)
    ## * Target: Species
    ## * Properties: multiclass
    ## * Features (4):
    ##   - dbl (4): Petal.Length, Petal.Width, Sepal.Length, Sepal.Width

``` r
# load learner and set hyperparameter
learner <- lrn("classif.rpart", cp = .01)
```

### Basic train + predict

``` r
# train/test split
train_set <- sample(task_iris$nrow, 0.8 * task_iris$nrow)
test_set <- setdiff(seq_len(task_iris$nrow), train_set)

# train the model
learner$train(task_iris, row_ids = train_set)

# predict data
prediction <- learner$predict(task_iris, row_ids = test_set)

# calculate performance
prediction$confusion
```

    ##             truth
    ## response     setosa versicolor virginica
    ##   setosa         11          0         0
    ##   versicolor      0         12         1
    ##   virginica       0          0         6

``` r
measure <- msr("classif.acc")
prediction$score(measure)
```

    ## classif.acc 
    ##   0.9666667

### Resample

``` r
# automatic resampling
resampling <- rsmp("cv", folds = 3L)
rr <- resample(task_iris, learner, resampling)
rr$score(measure)
```

    ##             task task_id               learner    learner_id     resampling
    ## 1: <TaskClassif>    iris <LearnerClassifRpart> classif.rpart <ResamplingCV>
    ## 2: <TaskClassif>    iris <LearnerClassifRpart> classif.rpart <ResamplingCV>
    ## 3: <TaskClassif>    iris <LearnerClassifRpart> classif.rpart <ResamplingCV>
    ##    resampling_id iteration prediction classif.acc
    ## 1:            cv         1     <list>        0.92
    ## 2:            cv         2     <list>        0.92
    ## 3:            cv         3     <list>        0.94

``` r
rr$aggregate(measure)
```

    ## classif.acc 
    ##   0.9266667

## Why a rewrite?

[mlr](https://github.com/mlr-org/mlr) was first released to
[CRAN](https://cran.r-project.org/package=mlr) in 2013. Its core design
and architecture date back even further. The addition of many features
has led to a [feature
creep](https://en.wikipedia.org/wiki/Feature_creep) which makes
[mlr](https://github.com/mlr-org/mlr) hard to maintain and hard to
extend. We also think that while mlr was nicely extensible in some parts
(learners, measures, etc.), other parts were less easy to extend from
the outside. Also, many helpful R libraries did not exist at the time
[mlr](https://github.com/mlr-org/mlr) was created, and their inclusion
would result in non-trivial API changes.

## Design principles

  - Only the basic building blocks for machine learning are implemented
    in this package.
  - Focus on computation here. No visualization or other stuff. That can
    go in extra packages.
  - Overcome the limitations of R’s [S3
    classes](https://adv-r.hadley.nz/s3.html) with the help of
    [R6](https://cran.r-project.org/package=R6).
  - Embrace [R6](https://cran.r-project.org/package=R6) for a clean
    OO-design, object state-changes and reference semantics. This might
    be less “traditional R”, but seems to fit `mlr` nicely.
  - Embrace
    [`data.table`](https://cran.r-project.org/package=data.table) for
    fast and convenient data frame computations.
  - Combine `data.table` and `R6`, for this we will make heavy use of
    list columns in data.tables.
  - Defensive programming and type safety. All user input is checked
    with [`checkmate`](https://cran.r-project.org/package=checkmate).
    Return types are documented, and mechanisms popular in base R which
    “simplify” the result unpredictably (e.g., `sapply()` or `drop`
    argument in `[.data.frame`) are avoided.
  - Be light on dependencies. `mlr3` requires the following packages at
    runtime:
      - [`future.apply`](https://cran.r-project.org/package=future.apply):
        Resampling and benchmarking is parallelized with the
        [`future`](https://cran.r-project.org/package=future)
        abstraction interfacing many parallel backends.
      - [`backports`](https://cran.r-project.org/package=backports):
        Ensures backward compatibility with older R releases. Developed
        by members of the `mlr` team. No recursive dependencies.
      - [`checkmate`](https://cran.r-project.org/package=checkmate):
        Fast argument checks. Developed by members of the `mlr` team. No
        extra recursive dependencies.
      - [`mlr3misc`](https://cran.r-project.org/package=mlr3misc):
        Miscellaneous functions used in multiple mlr3 [extension
        packages](https://github.com/mlr-org/mlr3/wiki/Extension-Packages).
        Developed by the `mlr` team. No extra recursive dependencies.
      - [`paradox`](https://cran.r-project.org/package=paradox):
        Descriptions for parameters and parameter sets. Developed by the
        `mlr` team. No extra recursive dependencies.
      - [`R6`](https://cran.r-project.org/package=R6): Reference class
        objects. No recursive dependencies.
      - [`data.table`](https://cran.r-project.org/package=data.table):
        Extension of R’s `data.frame`. No recursive dependencies.
      - [`digest`](https://cran.r-project.org/package=digest): Hash
        digests. No recursive dependencies.
      - [`uuid`](https://cran.r-project.org/package=uuid): Create unique
        string identifiers. No recursive dependencies.
      - [`lgr`](https://cran.r-project.org/package=lgr): Logging
        facility. No extra recursive dependencies.
      - [`mlr3measures`](https://cran.r-project.org/package=mlr3measures):
        Performance measures. No extra recursive dependencies.
      - [`mlbench`](https://cran.r-project.org/package=mlbench): A
        collection of machine learning data sets. No dependencies.
  - [Reflections](https://en.wikipedia.org/wiki/Reflection_%28computer_programming%29):
    Objects are queryable for properties and capabilities, allowing you
    to program on them.
  - Additional functionality that comes with extra dependencies:
      - To capture output, warnings and exceptions,
        [`evaluate`](https://cran.r-project.org/package=evaluate) and
        [`callr`](https://cran.r-project.org/package=callr) can be used.

## Extension Packages

<a href="https://raw.githubusercontent.com/mlr-org/mlr3/master/man/figures/mlr3verse.svg?sanitize=true"><img src="man/figures/mlr3verse.svg" /></a>

Consult the
[wiki](https://github.com/mlr-org/mlr3/wiki/Extension-Packages) for
short descriptions and links to the respective repositories.

## Contributing to mlr3

This R package is licensed under the
[LGPL-3](https://www.gnu.org/licenses/lgpl-3.0.en.html). If you
encounter problems using this software (lack of documentation,
misleading or wrong documentation, unexpected behaviour, bugs, …) or
just want to suggest features, please open an issue in the [issue
tracker](https://github.com/mlr-org/mlr3/issues). Pull requests are
welcome and will be included at the discretion of the maintainers.

Please consult the [wiki](https://github.com/mlr-org/mlr3/wiki/) for a
[style guide](https://github.com/mlr-org/mlr3/wiki/Style-Guide), a
[roxygen guide](https://github.com/mlr-org/mlr3/wiki/Roxygen-Guide) and
a [pull request
guide](https://github.com/mlr-org/mlr3/wiki/PR-Guidelines).

## Citing mlr3

If you use mlr3, please cite our [JOSS
article](https://doi.org/10.21105/joss.01903):

    @Article{mlr3,
      title = {{mlr3}: A modern object-oriented machine learning framework in {R}},
      author = {Michel Lang and Martin Binder and Jakob Richter and Patrick Schratz and Florian Pfisterer and Stefan Coors and Quay Au and Giuseppe Casalicchio and Lars Kotthoff and Bernd Bischl},
      journal = {Journal of Open Source Software},
      year = {2019},
      month = {dec},
      doi = {10.21105/joss.01903},
      url = {https://joss.theoj.org/papers/10.21105/joss.01903},
    }
